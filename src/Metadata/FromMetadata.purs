module Metadata.FromMetadata where

import Prelude

import Data.Array (toUnfoldable, uncons, foldMap) as Array
import Data.BigInt (BigInt)
import Data.Map (Map)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor.Strong ((***))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Types.ByteArray (ByteArray)
import Types.Int (Int, toBigInt) as Int
import Types.TransactionMetadata
  ( TransactionMetadatum(MetadataList, MetadataMap, Int, Bytes, Text)
  )

--------------------------------------------------------------------------------
-- FromMetadata
--------------------------------------------------------------------------------

class FromMetadata (a :: Type) where
  fromMetadata :: TransactionMetadatum -> Maybe a

instance FromMetadata TransactionMetadatum where
  fromMetadata = Just

instance FromMetadata a => FromMetadata (Array a) where
  fromMetadata = fromMetadataUnfoldable

instance FromMetadata a => FromMetadata (NonEmpty Array a) where
  fromMetadata md = do
    { head, tail } <- Array.uncons =<< fromMetadata md
    pure (head :| tail)

instance FromMetadata Int.Int where
  fromMetadata (Int n) = Just n
  fromMetadata _ = Nothing

instance FromMetadata BigInt where
  fromMetadata (Int n) = pure $ Int.toBigInt n
  fromMetadata _ = Nothing

instance FromMetadata ByteArray where
  fromMetadata (Bytes byteArr) = Just byteArr
  fromMetadata _ = Nothing

instance FromMetadata String where
  fromMetadata (Text str) = Just str
  fromMetadata _ = Nothing

instance (FromMetadata k, FromMetadata v, Ord k) => FromMetadata (Map k v) where
  fromMetadata (MetadataMap mp) =
    Just
      $ Map.fromFoldable
      $ Array.foldMap fromTupleMaybe
      $ (fromMetadata *** fromMetadata) <$> (Map.toUnfoldable mp)
  fromMetadata _ = Nothing

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

fromMetadataUnfoldable
  :: forall (a :: Type) (f :: Type -> Type)
   . Unfoldable f
  => FromMetadata a
  => TransactionMetadatum
  -> Maybe (f a)
fromMetadataUnfoldable (MetadataList entries) =
  Array.toUnfoldable <$> traverse fromMetadata entries
fromMetadataUnfoldable _ = Nothing

fromTupleMaybe
  :: forall (a :: Type) (b :: Type)
   . Tuple (Maybe a) (Maybe b)
  -> Array (Tuple a b)
fromTupleMaybe (Tuple (Just a) (Just b)) = [ Tuple a b ]
fromTupleMaybe (Tuple _ _) = []
