module CTL.Internal.Metadata.FromMetadata where

import Prelude

import CTL.Internal.Types.ByteArray (ByteArray)
import CTL.Internal.Types.Int (Int, toBigInt) as Int
import CTL.Internal.Types.TransactionMetadata
  ( TransactionMetadatum(MetadataList, Int, Bytes, Text)
  )
import Data.Array (toUnfoldable, uncons) as Array
import Data.BigInt (BigInt)
import Data.Maybe (Maybe(Just, Nothing))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (traverse)
import Data.Unfoldable (class Unfoldable)

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
