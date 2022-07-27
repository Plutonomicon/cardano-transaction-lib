module Metadata.Helpers
  ( mkKey
  , unsafeMkKey
  , lookupKey
  , lookupMetadata
  , errExpectedObject
  ) where

import Prelude

import Aeson (JsonDecodeError(TypeMismatch))
import Data.Either (Either(Left))
import Data.Foldable (lookup)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(Nothing), fromJust)
import Types.ByteArray (byteArrayFromAscii)
import Types.PlutusData (PlutusData(Map, Bytes))
import Types.TransactionMetadata (TransactionMetadatum(MetadataMap, Text))

mkKey :: String -> Maybe PlutusData
mkKey str = Bytes <$> byteArrayFromAscii str

unsafeMkKey :: Partial => String -> PlutusData
unsafeMkKey = fromJust <<< mkKey

lookupKey :: String -> PlutusData -> Maybe PlutusData
lookupKey keyStr (Map array) = mkKey keyStr >>= flip lookup array
lookupKey _ _ = Nothing

lookupMetadata :: String -> TransactionMetadatum -> Maybe TransactionMetadatum
lookupMetadata keyStr (MetadataMap mp) = Map.lookup (Text keyStr) mp
lookupMetadata _ _ = Nothing

errExpectedObject :: forall (a :: Type). Either JsonDecodeError a
errExpectedObject =
  Left (TypeMismatch "Expected object")
