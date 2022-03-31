module Seabug.Contract.MarketPlaceBuy
  ( marketplaceBuy
  , mkMarketplaceTx
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
  , otherData
  , otherScript
  , ownPaymentPubKeyHash
  , typedValidatorLookups
  , unspentOutputs
  ) as ScriptLookups
import Contract.Monad
  ( Contract
  , liftContractM
  , liftContractE'
  , liftedE'
  , liftedM
  )
import Contract.Numeric.Natural (toBigInt)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , toData
  , unitRedeemer
  )
import Contract.ProtocolParameters.Alonzo (minAdaTxOut)
import Contract.Scripts (typedValidatorEnterpriseAddress)
import Contract.Transaction
  ( TxOut
  , UnbalancedTx
  , balanceTx
  , finalizeTx
  , submitTransaction
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustMintValueWithRedeemer
  , mustPayToOtherScript
  , mustPayWithDatumToPubKey
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , coinToValue
  , lovelaceValueOf
  , mkSingletonValue'
  , scriptCurrencySymbol
  , valueOf
  )
import Data.Array (find) as Array
import Data.BigInt (BigInt, fromInt)
import Data.Map (insert, toUnfoldable)
import QueryM (FinalizedTransaction(FinalizedTransaction))
import QueryM.Submit (submit)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.MintingPolicy (mintingPolicy)
import Seabug.Token (mkTokenName)
import Seabug.Types
  ( MarketplaceDatum(MarketplaceDatum)
  , MintAct(ChangeOwner)
  , NftData(NftData)
  , NftId(NftId)
  )
import Types.ScriptLookups (mkUnbalancedTx')
import Types.Transaction (Redeemer) as T

marketplaceBuy :: NftData -> Contract Unit
marketplaceBuy nftData = do
  { unbalancedTx, datums, redeemers } /\ curr /\ newName <-
    mkMarketplaceTx nftData
  log "marketplaceBuy: Unbalanced transaction successfully built"
  log $ show unbalancedTx
  log =<< show <$> balanceTx unbalancedTx
  -- Balance unbalanced tx:
  balancedTx <- liftedE' $ balanceTx unbalancedTx
  log "marketplaceBuy: Transaction successfully balanced"
  log $ show (unwrap balancedTx).body
  log $ show (unwrap balancedTx).witness_set
  log $ show (unwrap balancedTx).is_valid
  -- Reattach datums and redeemer:
  FinalizedTransaction txCbor <-
    liftedM "marketplaceBuy: Cannot attach datums and redeemer"
      (finalizeTx balancedTx datums redeemers)
  log "marketplaceBuy: Datums and redeemer attached"
  -- Submit transaction:
  transactionHash <- wrap $ submit txCbor
  -- -- Submit balanced tx:
  -- transactionHash <- liftedM "marketplaceBuy: Failed transaction Submission"
  --   (submitTransaction txCbor)
  log $ "marketplaceBuy: Transaction successfully submitted with hash: "
    <> show transactionHash
  log $ "marketplaceBuy: Buy successful: " <> show (curr /\ newName)

-- https://github.com/mlabs-haskell/plutus-use-cases/blob/927eade6aa9ad37bf2e9acaf8a14ae2fc304b5ba/mlabs/src/Mlabs/EfficientNFT/Contract/MarketplaceBuy.hs
-- rev: 2c9ce295ccef4af3f3cb785982dfe554f8781541
-- The `MintingPolicy` may be decoded as Json, although I'm not sure as we don't
-- have `mkMintingPolicyScript`. Otherwise, it's an policy that hasn't been
-- applied to arguments. See `Seabug.Token.policy`
mkMarketplaceTx
  :: NftData
  -> Contract
       ( { unbalancedTx :: UnbalancedTx
         , datums :: Array Datum
         , redeemers :: Array T.Redeemer
         } /\ CurrencySymbol /\ TokenName
       )
mkMarketplaceTx (NftData nftData) = do
  pkh <- liftedM "marketplaceBuy: Cannot get PaymentPubKeyHash"
    ownPaymentPubKeyHash
  policy <- liftedE' $ pure mintingPolicy
  curr <- liftedM "marketplaceBuy: Cannot get CurrencySymbol"
    (scriptCurrencySymbol policy)
  -- Read in the typed validator:
  marketplaceValidator' <- unwrap <$> liftContractE' marketplaceValidator
  networkId <- getNetworkId
  let
    nft = nftData.nftId
    nft' = unwrap nft
    newNft = NftId nft' { owner = pkh }
    scriptAddr =
      typedValidatorEnterpriseAddress networkId $ wrap marketplaceValidator'
  oldName <- liftedM "marketplaceBuy: Cannot hash old token" (mkTokenName nft)
  newName <- liftedM "marketplaceBuy: Cannot hash new token" (mkTokenName newNft)
  -- Eventually we'll have a non-CSL-Plutus-style `Value` so this will likely
  -- change:
  oldNftValue <- liftContractM "marketplaceBuy: Cannot create old NFT Value"
    (mkSingletonValue' curr oldName $ negate one)
  newNftValue <- liftContractM "marketplaceBuy: Cannot create new NFT Value"
    (mkSingletonValue' curr newName one)
  log "oldNftValue"
  log $ show oldNftValue
  let
    nftPrice = nft'.price
    valHash = marketplaceValidator'.validatorHash
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
    userAddr = payPubKeyHashAddress networkId pkh
  userUtxos <-
    liftedM "marketplaceBuy: Cannot get user Utxos" (utxosAt userAddr)
  scriptUtxos <-
    liftedM "marketplaceBuy: Cannot get script Utxos" (utxosAt scriptAddr)
  let utxo' = Array.find containsNft $ toUnfoldable (unwrap scriptUtxos)
  utxo /\ utxoIndex <-
    liftContractM "marketplaceBuy: NFT not found on marketplace" utxo'
  log "hey"
  -- let x = utxoIndex.datum
  -- datumLookup <- liftedM "marketplaceBuy: Cannot hash datum for lookup"
  -- (ScriptLookups.otherData datum)
  log "ho"
  let
    lookup = mconcat
      [ ScriptLookups.mintingPolicy policy
      , ScriptLookups.typedValidatorLookups $ wrap marketplaceValidator'
      , ScriptLookups.otherScript marketplaceValidator'.validator
      , ScriptLookups.unspentOutputs $ insert utxo utxoIndex (unwrap userUtxos)
      , ScriptLookups.ownPaymentPubKeyHash pkh
      -- , datumLookup
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
  -- Created unbalanced tx which stripped datums and redeemers:
  txDatumsRedeemer <- liftedE' $ wrap (mkUnbalancedTx' lookup constraints)
  pure $ txDatumsRedeemer /\ curr /\ newName
