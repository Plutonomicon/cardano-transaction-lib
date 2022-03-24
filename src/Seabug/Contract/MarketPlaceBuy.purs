module Seabug.Contract.MarketPlaceBuy where

import Contract.Prelude
import Contract.Address (ownPaymentPubKeyHash)
import Contract.Monad (Contract, liftedM)
import Contract.Scripts (MintingPolicy, validatorAddress)
import Contract.Transaction (TransactionOutput)
import Contract.Value (scriptCurrencySymbol, valueOf)
import Effect.Exception (Error, error, throw)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Token (policy)
import Seabug.Types (NftData(NftData))

-- https://github.com/mlabs-haskell/plutus-use-cases/blob/927eade6aa9ad37bf2e9acaf8a14ae2fc304b5ba/mlabs/src/Mlabs/EfficientNFT/Contract/MarketplaceBuy.hs
-- rev: 2c9ce295ccef4af3f3cb785982dfe554f8781541
-- The `MintingPolicy` may be decoded as Json, although I'm not sure as we don't
-- have `mkMintingPolicyScript`. Otherwise, it's an policy that hasn't been
-- applied to arguments. See `Seabug.Token.policy`
marketplaceBuy :: NftData -> MintingPolicy -> Contract NftData
marketplaceBuy nftData@(NftData nftData') mp = do
  pkh <- liftedM "marketplaceBuy: Couldn't get PaymentPubKeyHash."
    ownPaymentPubKeyHash
  policy' <- liftedM "marketplaceBuy: Couldn't apply arguments."
    (policy nftData'.nftCollection mp)
  curr <- liftedM "marketplaceBuy: Couldn't get CurrencySymbol."
    (scriptCurrencySymbol policy')
  let
    marketplaceValidator' = unwrap marketplaceValidator
    nft = nftData'.nftId
  scriptAddr <- liftedM "marketplaceBuy: Couldn't get Address."
    (validatorAddress marketplaceValidator'.validator)
  -- let
    -- oldName = mkTokenName nft
    -- containsNft :: forall (a :: Type). (a /\ TransactionOutput) -> Boolean
    -- containsNft (_ /\ tx) = valueOf (unwrap tx).amount curr oldName == one
    -- containsNft (_, tx) = valueOf (_ciTxOutValue tx) curr oldName == 1
  --     valHash = validatorHash marketplaceValidator
  --     nftPrice = nftId'price nft
  --     newNft = nft {nftId'owner = pkh}
  --     oldName = mkTokenName nft
  --     newName = mkTokenName newNft
  --     oldNftValue = singleton curr oldName (-1)
  --     newNftValue = singleton curr newName 1
  --     mintRedeemer = Redeemer . toBuiltinData $ ChangeOwner nft pkh
  --     getShare share = (addExtend nftPrice * share) `divide` 10000
  --     authorShare = getShare (addExtend . nftCollection'authorShare . nftData'nftCollection $ nftData)
  --     daoShare = getShare (addExtend . nftCollection'daoShare . nftData'nftCollection $ nftData)
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