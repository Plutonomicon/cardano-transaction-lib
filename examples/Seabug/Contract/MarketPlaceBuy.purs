module Seabug.Contract.MarketPlaceBuy
  ( marketplaceBuy
  ) where

import Contract.Prelude
import Contract.Address
  ( getNetworkId
  , ownPaymentPubKeyHash
  , payPubKeyHashAddress
  )
import Contract.ScriptLookups
  ( mkUnbalancedTx
  , mintingPolicy
  , otherScript
  , ownPaymentPubKeyHash
  , typedValidatorLookups
  , unspentOutputs
  ) as ScriptLookups
import Contract.Monad (Contract, liftContractM, liftedE', liftedM)
import Contract.Numeric.Natural (toBigInt)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , toData
  , unitRedeemer
  )
import Contract.ProtocolParameters.Alonzo (minAdaTxOut)
import Contract.Scripts (validatorAddress)
import Contract.Transaction (TxOut, balanceTx, submitTransaction)
import Contract.TxConstraints
  ( TxConstraints
  , mustMintValueWithRedeemer
  , mustPayToOtherScript
  , mustPayWithDatumToPubKey
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value
  ( Value
  , coinToValue
  , lovelaceValueOf
  , mkSingletonValue'
  , scriptCurrencySymbol
  , valueOf
  )
import Data.Array (find) as Array
import Data.BigInt (BigInt, fromInt)
import Data.Map (insert, toUnfoldable)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Token (mkTokenName, policy, unappliedMintingPolicy)
import Seabug.Types
  ( MarketplaceDatum(MarketplaceDatum)
  , MintAct(ChangeOwner)
  , NftData(NftData)
  , NftId(NftId)
  )

-- https://github.com/mlabs-haskell/plutus-use-cases/blob/927eade6aa9ad37bf2e9acaf8a14ae2fc304b5ba/mlabs/src/Mlabs/EfficientNFT/Contract/MarketplaceBuy.hs
-- rev: 2c9ce295ccef4af3f3cb785982dfe554f8781541
-- The `MintingPolicy` may be decoded as Json, although I'm not sure as we don't
-- have `mkMintingPolicyScript`. Otherwise, it's an policy that hasn't been
-- applied to arguments. See `Seabug.Token.policy`
marketplaceBuy :: NftData -> Contract NftData
marketplaceBuy (NftData nftData) = do
  -- Read in the unapplied minting policy:
  mp <- liftedE' $ liftAff unappliedMintingPolicy
  pkh <- liftedM "marketplaceBuy: Cannot get PaymentPubKeyHash"
    ownPaymentPubKeyHash
  policy' <- liftedM "marketplaceBuy: Cannot apply arguments"
    (policy nftData.nftCollection mp)
  curr <- liftedM "marketplaceBuy: Cannot get CurrencySymbol"
    (scriptCurrencySymbol policy')
  -- Reader in the typed validator:
  marketplaceValidator' <- liftedE' $ liftAff marketplaceValidator
  let
    marketplaceValidator'' = unwrap marketplaceValidator'
    nft = nftData.nftId
    nft' = unwrap nft
    newNft = NftId nft' { owner = pkh }
  scriptAddr <- liftedM "marketplaceBuy: Cannot get script Address"
    (validatorAddress marketplaceValidator''.validator)
  oldName <- liftedM "marketplaceBuy: Cannot hash old token" (mkTokenName nft)
  newName <- liftedM "marketplaceBuy: Cannot hash new token" (mkTokenName newNft)
  -- Eventually we'll have a non-CSL-Plutus-style `Value` so this will likely
  -- change:
  oldNftValue <- liftContractM "marketplaceBuy: Cannot create old NFT Value"
    (mkSingletonValue' curr oldName $ negate one)
  newNftValue <- liftContractM "marketplaceBuy: Cannot create new NFT Value"
    (mkSingletonValue' curr newName one)
  networkId <- getNetworkId
  let
    nftPrice = nft'.price
    valHash = marketplaceValidator''.validatorHash
    mintRedeemer = Redeemer $ toData $ ChangeOwner nft pkh
    nftCollection = unwrap nftData.nftCollection

    containsNft :: forall (a :: Type). (a /\ TxOut) -> Boolean
    containsNft (_ /\ tx) = valueOf (unwrap tx).amount curr oldName == one

    getShare :: BigInt -> BigInt
    getShare share = (toBigInt nftPrice * share) `div` fromInt 10_000

    shareToSubtract :: BigInt -> BigInt
    shareToSubtract v
      | v < unwrap minAdaTxOut = zero
      | otherwise = v

    filterLowValue
      :: BigInt
      -> (Value -> TxConstraints Unit Unit)
      -> TxConstraints Unit Unit
    filterLowValue v t
      | v < unwrap minAdaTxOut = mempty
      | otherwise = t (lovelaceValueOf v)

    authorShare = getShare $ toBigInt nftCollection.authorShare
    daoShare = getShare $ toBigInt nftCollection.daoShare
    ownerShare = lovelaceValueOf
      $ toBigInt nftPrice
      - shareToSubtract authorShare
      - shareToSubtract daoShare
    datum = Datum $ toData $ curr /\ oldName
    newNftData =
      NftData { nftCollection: nftData.nftCollection, nftId: newNft }
    userAddr = payPubKeyHashAddress networkId pkh
  userUtxos <-
    liftedM "marketplaceBuy: Cannot get user Utxos" (utxosAt userAddr)
  scriptUtxos <-
    liftedM "marketplaceBuy: Cannot get script Utxos" (utxosAt scriptAddr)
  let utxo' = Array.find containsNft $ toUnfoldable (unwrap scriptUtxos)
  utxo /\ utxoIndex <-
    liftContractM "marketplaceBuy: NFT not found on marketplace" utxo'
  let
    lookup = mconcat
      [ ScriptLookups.mintingPolicy policy'
      , ScriptLookups.typedValidatorLookups marketplaceValidator'
      , ScriptLookups.otherScript marketplaceValidator''.validator
      , ScriptLookups.unspentOutputs $ insert utxo utxoIndex (unwrap userUtxos)
      , ScriptLookups.ownPaymentPubKeyHash pkh
      ]
    constraints =
      filterLowValue
        daoShare
        (mustPayToOtherScript nftCollection.daoScript datum)
        <> filterLowValue
          authorShare
          (mustPayWithDatumToPubKey nftCollection.author datum)
        <> mconcat
          [ mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , mustSpendScriptOutput utxo unitRedeemer
          , mustPayWithDatumToPubKey nft'.owner datum ownerShare
          , mustPayToOtherScript
              valHash
              ( Datum $ toData $
                  MarketplaceDatum { getMarketplaceDatum: curr /\ newName }
              )
              (newNftValue <> coinToValue minAdaTxOut)
          ]
  -- Created unbalanced tx:
  unbalancedTx <- liftedE' $ ScriptLookups.mkUnbalancedTx lookup constraints
  log "marketplaceBuy: Unbalanced transaction successfully built"
  -- Balance unbalanced tx:
  balancedTx <- liftedE' $ balanceTx unbalancedTx
  log "marketplaceBuy: Transaction successfully balanced"
  -- Submit balanced tx:
  transactionHash <- liftedM "marketplaceBuy: Failed transaction Submission"
    (submitTransaction balancedTx)
  log $ "marketplaceBuy: Transaction successfully submitted with hash: "
    <> show transactionHash
  log $ "marketplaceBuy: Buy successful: " <> show (curr /\ newName)
  -- As far as I can tell, we don't actually need the return value:
  pure newNftData
