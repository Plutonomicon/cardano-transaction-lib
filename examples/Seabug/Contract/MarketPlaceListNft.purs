-- | Helper to list the utxo with relevant NFT at the market validator script
module Seabug.Contract.MarketPlaceListNft
  ( marketPlaceListNft
  ) where

import Contract.Prelude
import Contract.Address (getNetworkId, typedValidatorEnterpriseAddress)
import Contract.Monad (Contract, liftContractE', liftedM)
import Contract.PlutusData (fromData, getDatumByHash)
import Contract.Transaction (TransactionOutput(TransactionOutput))
import Contract.Utxos (UtxoM, utxosAt)
import Contract.Value (valueOf)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Types (MarketplaceDatum(MarketplaceDatum))

-- | Lists the utxos at the script address that contain a datum of type
-- | `MarketplaceDatum` with unit value. It currently doesn't have any logic
-- | on matching `CurrencySymbol` and `TokenName`.
marketPlaceListNft :: Contract UtxoM
marketPlaceListNft = do
  marketplaceValidator' <- unwrap <$> liftContractE' marketplaceValidator
  networkId <- getNetworkId
  let
    scriptAddr =
      typedValidatorEnterpriseAddress networkId $ wrap marketplaceValidator'
  scriptUtxos <-
    liftedM "marketPlaceListNft: Cannot get script Utxos" (utxosAt scriptAddr)
  let
    -- Given a `TxOut`/`TransactionOutput`, returns a `Boolean` on whether the
    -- datum is of type `MarketplaceDatum` and unit value. We don't have access
    -- to NftData (or NftCollection) so we presumably can't get the currency
    -- symbol to match on?
    containsNft :: TransactionOutput -> Contract Boolean
    containsNft
      (TransactionOutput { amount, data_hash: Just datumHash }) = do
      plutusData <- liftedM "marketPlaceListNft: Cannot get datum from hash"
        (getDatumByHash datumHash)
      pure case fromData plutusData of
        Just (MarketplaceDatum { getMarketplaceDatum: curr /\ name }) ->
          -- Simplified logic without checking currency symbol matches as we
          -- can't get a fully applied policy.
          valueOf amount curr name == one
        _ -> false
    containsNft _ = pure false
  wrap <$> filterMapM containsNft (unwrap scriptUtxos)
