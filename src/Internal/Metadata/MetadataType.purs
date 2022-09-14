module CTL.Internal.Metadata.MetadataType
  ( class MetadataType
  , metadataLabel
  , fromGeneralTxMetadata
  , toGeneralTxMetadata
  ) where

import Prelude

import CTL.Internal.Metadata.FromMetadata (class FromMetadata, fromMetadata)
import CTL.Internal.Metadata.ToMetadata (class ToMetadata, toMetadata)
import CTL.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata
  , TransactionMetadatumLabel
  )
import Data.Map (lookup, singleton) as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Type.Proxy (Proxy(Proxy))

class (FromMetadata a, ToMetadata a) <= MetadataType (a :: Type) where
  metadataLabel :: Proxy a -> TransactionMetadatumLabel

fromGeneralTxMetadata
  :: forall (a :: Type). MetadataType a => GeneralTransactionMetadata -> Maybe a
fromGeneralTxMetadata =
  fromMetadata <=< Map.lookup (metadataLabel (Proxy :: Proxy a)) <<< unwrap

toGeneralTxMetadata
  :: forall (a :: Type). MetadataType a => a -> GeneralTransactionMetadata
toGeneralTxMetadata =
  wrap <<< Map.singleton (metadataLabel (Proxy :: Proxy a)) <<< toMetadata
