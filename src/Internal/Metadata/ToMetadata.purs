module Ctl.Internal.Metadata.ToMetadata
  ( class ToMetadata
  , toMetadata
  ) where

import Prelude

import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.Int (Int, fromBigInt) as Int
import Ctl.Internal.Types.TransactionMetadata
  ( TransactionMetadatum(MetadataMap, MetadataList, Int, Bytes, Text)
  )
import Data.Map (Map)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Maybe (fromJust)
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple)
import JS.BigInt (BigInt)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- ToMetadata
--------------------------------------------------------------------------------

class ToMetadata (a :: Type) where
  toMetadata :: a -> TransactionMetadatum

instance ToMetadata TransactionMetadatum where
  toMetadata = identity

instance (ToMetadata k, ToMetadata v) => ToMetadata (Map k v) where
  toMetadata mp =
    let
      entries = Map.toUnfoldable mp :: Array (Tuple k v)
    in
      MetadataMap <<< Map.fromFoldable $
        map (toMetadata *** toMetadata) entries

instance
  ( Ord k
  , ToMetadata k
  , ToMetadata v
  ) =>
  ToMetadata (Array (Tuple k v)) where
  toMetadata = toMetadata <<< Map.fromFoldable
else instance ToMetadata a => ToMetadata (Array a) where
  toMetadata = MetadataList <<< map toMetadata

instance ToMetadata Int.Int where
  toMetadata = Int

-- FIXME: Come up with a type-safe error handling approach.
instance ToMetadata BigInt where
  toMetadata bi =
    unsafePartial $ Int $ fromJust $ Int.fromBigInt bi

instance ToMetadata ByteArray where
  toMetadata = Bytes

instance ToMetadata String where
  toMetadata = Text
