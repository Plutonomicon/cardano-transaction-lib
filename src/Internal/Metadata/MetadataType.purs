module Ctl.Internal.Metadata.MetadataType
  ( class MetadataType
  , metadataLabel
  , fromGeneralTxMetadata
  , toGeneralTxMetadata
  ) where

import Prelude

import Cardano.FromMetadata (class FromMetadata, fromMetadata)
import Cardano.ToMetadata (class ToMetadata, toMetadata)
import Cardano.Types (GeneralTransactionMetadata)
import Ctl.Internal.Types.MetadataLabel (MetadataLabel)
import Data.Map (lookup, singleton) as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Type.Proxy (Proxy(Proxy))

-- | Associates a metadata label with a type. E.g. a CIP-25 metadata type could be associated with label `721`.
class (FromMetadata a, ToMetadata a) <= MetadataType (a :: Type) where
  metadataLabel :: Proxy a -> MetadataLabel

fromGeneralTxMetadata
  :: forall (a :: Type). MetadataType a => GeneralTransactionMetadata -> Maybe a
fromGeneralTxMetadata =
  fromMetadata <=< Map.lookup (unwrap $ metadataLabel (Proxy :: Proxy a)) <<<
    unwrap

toGeneralTxMetadata
  :: forall (a :: Type). MetadataType a => a -> GeneralTransactionMetadata
toGeneralTxMetadata =
  wrap <<< Map.singleton (unwrap $ metadataLabel (Proxy :: Proxy a)) <<<
    toMetadata
