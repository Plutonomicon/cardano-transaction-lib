module Cardano.Types.GeneralTransactionMetadata where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.TransactionMetadatum (TransactionMetadatum)
import Ctl.Internal.Helpers (appendRightMap, encodeMap)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype GeneralTransactionMetadata =
  GeneralTransactionMetadata
    (Map BigNum TransactionMetadatum)

derive instance Newtype GeneralTransactionMetadata _

derive newtype instance Eq GeneralTransactionMetadata
derive instance Generic GeneralTransactionMetadata _

instance Show GeneralTransactionMetadata where
  show = genericShow

instance EncodeAeson GeneralTransactionMetadata where
  encodeAeson (GeneralTransactionMetadata m) = encodeMap m

-- This Semigroup instance simply takes the Last value for duplicate keys
-- to avoid a Semigroup instance for TransactionMetadatum.
-- Do we want to avoid a Semigroup instance for TransactionMetadatum? Recursion
-- is fine but how to combine Text with Bytes for example? One would have to take
-- precedence and replace the other.
instance Semigroup GeneralTransactionMetadata where
  append (GeneralTransactionMetadata hm) (GeneralTransactionMetadata hm') =
    GeneralTransactionMetadata $ hm `appendRightMap` hm'

instance Monoid GeneralTransactionMetadata where
  mempty = GeneralTransactionMetadata Map.empty
