-- | Helper to list the utxo with relevant NFT at the market validator script
module Seabug.Contract.MarketPlaceListNft
  ( ListNftResult
  , marketPlaceListNft
  ) where

import Contract.Prelude
import Contract.Address (getNetworkId, typedValidatorEnterpriseAddress)
import Contract.Monad (Contract, liftContractE', liftedM)
import Contract.PlutusData (fromData, getDatumByHash)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  )
import Contract.Utxos (utxosAt)
import Contract.Value (valueOf)
import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Data.Array (catMaybes)
import Data.Map as Map
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Metadata (FullSeabugMetadata, getFullSeabugMetadata)
import Seabug.Types (MarketplaceDatum(MarketplaceDatum))

type ListNftResult =
  { input :: TransactionInput
  , output :: TransactionOutput
  , metadata :: FullSeabugMetadata
  }

-- | Lists the utxos at the script address that contain a datum of type
-- | `MarketplaceDatum` with unit value. It currently doesn't have any logic
-- | on matching `CurrencySymbol` and `TokenName`.
marketPlaceListNft :: Contract (Array ListNftResult)
marketPlaceListNft = do
  marketplaceValidator' <- unwrap <$> liftContractE' marketplaceValidator
  networkId <- getNetworkId
  let
    scriptAddr =
      typedValidatorEnterpriseAddress networkId $ wrap marketplaceValidator'
  scriptUtxos <- Map.toUnfoldable <<< unwrap <$>
    liftedM "marketPlaceListNft: Cannot get script Utxos" (utxosAt scriptAddr)
  withMetadata <- for scriptUtxos $ \(input /\ output@(TransactionOutput out)) ->
    runMaybeT $ do
      datumHash <- MaybeT $ pure $ out.data_hash
      plutusData <- MaybeT $ getDatumByHash datumHash
      MarketplaceDatum { getMarketplaceDatum: curr /\ name } <-
        MaybeT $ pure $ fromData plutusData
      guard $ valueOf out.amount curr name == one
      metadata <- MaybeT $ map hush $ getFullSeabugMetadata $ curr /\ name
      pure { input, output, metadata }
  pure $ catMaybes withMetadata
