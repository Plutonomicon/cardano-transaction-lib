module Metadata.ToMetadata
  ( class ToMetadata
  , toMetadata
  , AnyToMetadata
  , class AnyToMetadataClass
  , anyToMetadata
  ) where

import Prelude

import Data.Array (fromFoldable) as Array
import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map (catMaybes, fromFoldable, singleton, toUnfoldable) as Map
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty)
import Data.Profunctor.Strong ((***))
import Data.Tuple.Nested (type (/\))
import Types.ByteArray (ByteArray)
import Types.Int (Int) as Int
import Types.TransactionMetadata
  ( GeneralTransactionMetadata
  , TransactionMetadatum(MetadataMap, MetadataList, Int, Bytes, Text)
  , TransactionMetadatumLabel
  )

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
      entries = Map.toUnfoldable mp :: Array (k /\ v)
    in
      MetadataMap <<< Map.fromFoldable $
        map (toMetadata *** toMetadata) entries

instance (Ord k, ToMetadata k) => ToMetadata (Array (k /\ AnyToMetadata)) where
  toMetadata = toMetadata <<< Map.fromFoldable
else instance (Ord k, ToMetadata k, ToMetadata v) => ToMetadata (Array (k /\ v)) where
  toMetadata = toMetadata <<< Map.fromFoldable
else instance ToMetadata a => ToMetadata (Array a) where
  toMetadata = MetadataList <<< map toMetadata

instance (Foldable f, ToMetadata a) => ToMetadata (NonEmpty f a) where
  toMetadata = toMetadata <<< Array.fromFoldable

instance ToMetadata Int.Int where
  toMetadata = Int

instance ToMetadata ByteArray where
  toMetadata = Bytes

instance ToMetadata String where
  toMetadata = Text

--------------------------------------------------------------------------------
-- AnyToMetadata
--------------------------------------------------------------------------------

newtype AnyToMetadata = AnyToMetadata
  (forall (r :: Type). (forall (a :: Type). ToMetadata a => a -> r) -> Maybe r)

class AnyToMetadataClass (a :: Type) where
  anyToMetadata :: a -> AnyToMetadata

instance ToMetadata a => AnyToMetadataClass (Maybe a) where
  anyToMetadata a = AnyToMetadata \f -> f <$> a
else instance ToMetadata a => AnyToMetadataClass a where
  anyToMetadata a = AnyToMetadata \f -> Just (f a)
