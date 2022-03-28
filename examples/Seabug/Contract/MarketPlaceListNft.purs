-- | Helper to list the utxo with relevant NFT at the market validator script
module Seabug.Contract.MarketPlaceListNft
  ( marketPlaceListNft
  ) where

import Contract.Prelude
import Contract.Address (validatorAddress)
import Contract.Monad (Contract, liftContractE', liftedE', liftedM)
import Contract.PlutusData (fromData, getDatumByHash)
import Contract.Transaction (TransactionOutput(TransactionOutput))
import Contract.Utxos (UtxoM, utxosAt)
import Contract.Value (scriptCurrencySymbol, valueOf)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Token (mkTokenName, policy, unappliedMintingPolicy)
import Seabug.Types (MarketplaceDatum(MarketplaceDatum), NftData(NftData))

-- | Lists the utxos at the script address that contain a datum of type
-- | `NftData` with the correct NFT
marketPlaceListNft :: NftData -> Contract UtxoM
marketPlaceListNft (NftData nftData) = do
  marketplaceValidator' <- unwrap <$> liftContractE' marketplaceValidator
  scriptAddr <- liftedM "marketPlaceListNft: Cannot get script Address"
    (validatorAddress marketplaceValidator'.validator)
  scriptUtxos <-
    liftedM "marketPlaceListNft: Cannot get script Utxos" (utxosAt scriptAddr)
  -- Get the applied minting policy before hand so we don't repeat the same code
  -- inside `containsNft`
  mp <- liftedE' $ pure unappliedMintingPolicy
  let
    -- Given a `TxOut`/`TransactionOutput`, returns a `Boolean` on whether the
    -- datum is of type `NftData` and has the correct NFT value.
    containsNft :: TransactionOutput -> Contract Boolean
    containsNft
      (TransactionOutput { amount, data_hash: Just datumHash }) = do
      plutusData <- liftedM "marketPlaceListNft: Cannot get datum from hash"
        (getDatumByHash datumHash)
      case fromData plutusData of
        Just (MarketplaceDatum { getMarketplaceDatum: curr' /\ name' }) -> do
          policy' <- liftedM "marketPlaceListNft: Cannot apply arguments"
            (policy nftData.nftCollection mp)
          curr <- liftedM "marketPlaceListNft: Cannot get CurrencySymbol"
            (scriptCurrencySymbol policy')
          name <- liftedM "marketPlaceListNft: Cannot hash token"
            (mkTokenName nftData.nftId)
          pure
            $ curr
            == curr'
            && name
            == name'
            && valueOf amount curr name
            == one
        _ -> pure false
    containsNft _ = pure false
  wrap <$> filterMapM containsNft (unwrap scriptUtxos)
