module Metadata.Helpers
  ( mkKey
  , lookupKey
  ) where

import Prelude

import Data.Maybe (Maybe, fromJust)
import Data.Map (Map, lookup)
import Types.ByteArray (byteArrayFromString)
import Types.PlutusData (PlutusData(Bytes))

mkKey :: Partial => String -> PlutusData
mkKey str = Bytes $ fromJust $ byteArrayFromString str

lookupKey :: Partial => String -> Map PlutusData PlutusData -> Maybe PlutusData
lookupKey keyStr = lookup (mkKey keyStr)
