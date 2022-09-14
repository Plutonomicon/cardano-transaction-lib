module CTL.Internal.Metadata.ToMetadata
  ( class ToMetadata
  , toMetadata
  , AnyToMetadata
  , class AnyToMetadataClass
  , anyToMetadata
  ) where

import Prelude

import CTL.Internal.Types.ByteArray (ByteArray)
import CTL.Internal.Types.Int (Int, fromBigInt) as Int
import CTL.Internal.Types.TransactionMetadata
  ( TransactionMetadatum(MetadataMap, MetadataList, Int, Bytes, Text)
  )
import Data.Array (fromFoldable) as Array
import Data.BigInt (BigInt)
import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map (catMaybes, fromFoldable, toUnfoldable) as Map
import Data.Maybe (Maybe(Just), fromJust)
import Data.NonEmpty (NonEmpty)
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- ToMetadata
--------------------------------------------------------------------------------

class ToMetadata (a :: Type) where
  toMetadata :: a -> TransactionMetadatum

instance ToMetadata TransactionMetadatum where
  toMetadata = identity

instance (Ord k, ToMetadata k) => ToMetadata (Map k AnyToMetadata) where
  toMetadata =
    toMetadata <<< Map.catMaybes <<< map (\(AnyToMetadata f) -> f toMetadata)
else instance (ToMetadata k, ToMetadata v) => ToMetadata (Map k v) where
  toMetadata mp =
    let
      entries = Map.toUnfoldable mp :: Array (Tuple k v)
    in
      MetadataMap <<< Map.fromFoldable $
        map (toMetadata *** toMetadata) entries

instance (Ord k, ToMetadata k) => ToMetadata (Array (Tuple k AnyToMetadata)) where
  toMetadata = toMetadata <<< Map.fromFoldable
else instance
  ( Ord k
  , ToMetadata k
  , ToMetadata v
  ) =>
  ToMetadata (Array (Tuple k v)) where
  toMetadata = toMetadata <<< Map.fromFoldable
else instance ToMetadata a => ToMetadata (Array a) where
  toMetadata = MetadataList <<< map toMetadata

instance (Foldable f, ToMetadata a) => ToMetadata (NonEmpty f a) where
  toMetadata = toMetadata <<< Array.fromFoldable

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

--------------------------------------------------------------------------------
-- AnyToMetadata
--------------------------------------------------------------------------------

-- | Existential wrapper over `ToMetadata` constrained types that enables
-- | heterogeneous collections and is particularly useful when dealing with
-- | `Maybe` values. `TransactionMetadatum` doesn't provide a way to
-- | represent `Maybe` values, so the basic idea is to filter out all
-- | `Nothing`s during conversion (see `ToMetadata (Map k AnyToMetadata)`
-- | instance for reference).
newtype AnyToMetadata = AnyToMetadata
  (forall (r :: Type). (forall (a :: Type). ToMetadata a => a -> r) -> Maybe r)

class AnyToMetadataClass (a :: Type) where
  anyToMetadata :: a -> AnyToMetadata

instance ToMetadata a => AnyToMetadataClass (Maybe a) where
  anyToMetadata a = AnyToMetadata \f -> f <$> a
else instance ToMetadata a => AnyToMetadataClass a where
  anyToMetadata a = AnyToMetadata \f -> Just (f a)
