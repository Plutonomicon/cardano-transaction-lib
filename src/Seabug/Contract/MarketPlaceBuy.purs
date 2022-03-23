module Seabug.Contract.MarketPlaceBuy where

import Contract.Prelude
import Contract.Address (ownPaymentPubKeyHash)
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Numeric.Natural (toBigInt)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.Scripts (MintingPolicy, validatorAddress)
import Contract.Transaction (TransactionOutput)
import Contract.Value (mkSingletonValue', scriptCurrencySymbol, valueOf)
import Data.BigInt (BigInt, fromInt)
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
  scriptAddr <- liftedM "marketplaceBuy: Cannot get Address."
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
  let
    nftPrice = nft'.price
    valHash = marketplaceValidator'.validatorHash
    mintRedeemer = Redeemer $ toData $ ChangeOwner nft pkh

    containsNft :: forall (a :: Type). (a /\ TransactionOutput) -> Boolean
    containsNft (_ /\ tx) = valueOf (unwrap tx).amount curr oldName == one

    getShare :: BigInt -> BigInt
    getShare share = (toBigInt nftPrice * share) `div` fromInt 10_000

    authorShare = getShare (toBigInt $ (unwrap nftData'.nftCollection).authorShare)
    daoShare = getShare (toBigInt $ (unwrap nftData'.nftCollection).daoShare)
  --     shareToSubtract v
  --       | v < getLovelace minAdaTxOut = 0
  --       | otherwise = v
  --     ownerShare = lovelaceValueOf (addExtend nftPrice - shareToSubtract authorShare - shareToSubtract daoShare)
  --     datum = Datum . toBuiltinData $ (curr, oldName)
  --     filterLowValue v t
  --       | v < getLovelace minAdaTxOut = mempty
  --       | otherwise = t (lovelaceValueOf v)
  --     newNftData = NftData (nftData'nftCollection nftData) newNft
  -- userUtxos <- getUserUtxos
  -- utxo' <- find containsNft . Map.toList <$> getAddrUtxos scriptAddr
  -- (utxo, utxoIndex) <- case utxo' of
  --   Nothing -> Contract.throwError "NFT not found on marketplace"
  --   Just x -> Hask.pure x
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