module Seabug.Contract.MarketPlaceBuy where

import Contract.Prelude
import Contract.Address (ownPaymentPubKeyHash, payPubKeyHashAddress)
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Numeric.Natural (toBigInt)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.ProtocolParameters.Alonzo (minAdaTxOut)
import Contract.Scripts (MintingPolicy, validatorAddress)
import Contract.Transaction (TransactionOutput)
import Contract.Utxos (utxosAt)
import Contract.Value
  ( lovelaceValueOf
  , mkSingletonValue'
  , scriptCurrencySymbol
  , valueOf
  )
import Control.Monad.Reader.Class (asks)
import Data.Array (find)
import Data.BigInt (BigInt, fromInt)
import Data.Map (toUnfoldable)
import Effect.Exception (Error, error, throw)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Token (mkTokenName, policy)
import Seabug.Types (MintAct(ChangeOwner), NftData(NftData), NftId(NftId))

-- https://github.com/mlabs-haskell/plutus-use-cases/blob/927eade6aa9ad37bf2e9acaf8a14ae2fc304b5ba/mlabs/src/Mlabs/EfficientNFT/Contract/MarketplaceBuy.hs
-- rev: 2c9ce295ccef4af3f3cb785982dfe554f8781541
-- The `MintingPolicy` may be decoded as Json, although I'm not sure as we don't
-- have `mkMintingPolicyScript`. Otherwise, it's an policy that hasn't been
-- applied to arguments. See `Seabug.Token.policy`
marketplaceBuy :: NftData -> MintingPolicy -> Contract NftData
marketplaceBuy nftData@(NftData nftData') mp = do
  pkh <- liftedM "marketplaceBuy: Cannot get PaymentPubKeyHash."
    ownPaymentPubKeyHash
  policy' <- liftedM "marketplaceBuy: Cannot apply arguments."
    (policy nftData'.nftCollection mp)
  curr <- liftedM "marketplaceBuy: Cannot get CurrencySymbol."
    (scriptCurrencySymbol policy')
  let
    marketplaceValidator' = unwrap marketplaceValidator
    nft = nftData'.nftId
    nft' = unwrap nft
    newNft = NftId nft' { owner = pkh }
  scriptAddr <- liftedM "marketplaceBuy: Cannot get script Address."
    (validatorAddress marketplaceValidator'.validator)
  oldName <-
    liftContractM "marketplaceBuy: Cannot hash old token." (mkTokenName nft)
  newName <-
    liftContractM "marketplaceBuy: Cannot hash new token." (mkTokenName newNft)
  -- Eventually we'll have a non-CSL-Plutus-style `Value` so this will likely
  -- change:
  oldNftValue <- liftContractM "marketplaceBuy: Cannot create old NFT Value."
    (mkSingletonValue' curr oldName $ negate one)
  newNftValue <- liftContractM "marketplaceBuy: Cannot create new NFT Value."
    (mkSingletonValue' curr newName one)
  networkId <- asks (unwrap >>> _.networkId)
  let
    nftPrice = nft'.price
    valHash = marketplaceValidator'.validatorHash
    mintRedeemer = Redeemer $ toData $ ChangeOwner nft pkh
    nftCollection = unwrap nftData'.nftCollection

    containsNft :: forall (a :: Type). (a /\ TransactionOutput) -> Boolean
    containsNft (_ /\ tx) = valueOf (unwrap tx).amount curr oldName == one

    getShare :: BigInt -> BigInt
    getShare share = (toBigInt nftPrice * share) `div` fromInt 10_000

    shareToSubtract :: BigInt -> BigInt
    shareToSubtract v
        | v < unwrap minAdaTxOut = zero
        | otherwise = v

    -- filterLowValue :: BigInt -> BigInt -> Value
    -- filterLowValue v t
    --   | v < unwrap minAdaTxOut = mempty
    --   | otherwise = t (lovelaceValueOf v)

    authorShare = getShare $ toBigInt nftCollection.authorShare
    daoShare = getShare $ toBigInt nftCollection.daoShare

    ownerShare = lovelaceValueOf $
      toBigInt nftPrice - shareToSubtract authorShare - shareToSubtract daoShare
    datum = Datum $ toData $ curr /\ oldName -- CHECK Tuple implementation
    newNftData =
      NftData { nftCollection: (nftData'.nftCollection), nftId: newNft}

    userAddr = payPubKeyHashAddress networkId pkh
  userUtxos <-
    liftedM "marketplaceBuy: Cannot get user Utxos." (utxosAt userAddr)
  scriptUtxos <-
    liftedM "marketplaceBuy: Cannot get script Utxos." (utxosAt scriptAddr)
  let utxo' = find containsNft $ toUnfoldable (unwrap scriptUtxos)
  utxo /\ utxoIndex <-
    liftContractM "marketplaceBuy: NFT not found on marketplace" utxo'
  -- let lookup =
  --       Hask.mconcat
  --         [ Constraints.mintingPolicy policy'
  --         , Constraints.typedValidatorLookups marketplaceValidator
  --         , Constraints.otherScript (validatorScript marketplaceValidator)
  --         , Constraints.unspentOutputs $ Map.insert utxo utxoIndex userUtxos
  --         , Constraints.ownPaymentPubKeyHash pkh
  --         ]
  --     tx =
  --       filterLowValue
  --         daoShare
  --         (Constraints.mustPayToOtherScript (nftCollection'daoScript . nftData'nftCollection $ nftData) datum)
  --         <> filterLowValue
  --           authorShare
  --           (Constraints.mustPayWithDatumToPubKey (nftCollection'author . nftData'nftCollection $ nftData) datum)
  --         <> Hask.mconcat
  --           [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
  --           , Constraints.mustSpendScriptOutput utxo (Redeemer $ toBuiltinData ())
  --           , Constraints.mustPayWithDatumToPubKey (nftId'owner nft) datum ownerShare
  --           , Constraints.mustPayToOtherScript
  --               valHash
  --               (Datum . toBuiltinData . MarketplaceDatum $ assetClass curr newName)
  --               (newNftValue <> toValue minAdaTxOut)
  --           ]
  -- void $ Contract.submitTxConstraintsWith @Any lookup tx
  -- Contract.tell . Hask.pure $ newNftData
  -- Contract.logInfo @Hask.String $ printf "Buy successful: %s" (Hask.show $ assetClass curr newName)
  -- Hask.pure newNftData

  pure nftData