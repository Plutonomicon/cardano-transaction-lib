module Types.PlutusData
  ( PlutusData(Constr, Map, List, Integer, Bytes)
  ) where

import Prelude

import Types.ByteArray (ByteArray)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe
import Data.Either
import Data.Show.Generic (genericShow)
import Data.Argonaut
import Data.Argonaut.Decode
import Undefined
import Data.Traversable
import Data.Tuple.Nested
import Data.Map as Map
import Control.Alt
import Types.ByteArray
import Aeson

data PlutusData
  = Constr BigInt (Array PlutusData)
  | Map (Map PlutusData PlutusData)
  | List (Array PlutusData)
  | Integer BigInt
  | Bytes ByteArray

derive instance Eq PlutusData
derive instance Ord PlutusData
derive instance Generic PlutusData _

instance Show PlutusData where
  show x = genericShow x

instance DecodeAeson PlutusData where
  decodeAeson index json = decodeConstr <|> decodeMap <|> decodeList <|> decodeInteger
    where
    decodeConstr = do
      x <- hush $ decodeJson json
      constrStr <- x .: "constr" -- mandatory field
      constr <- BigInt.fromString constrStr
      fields <- x .: "fields"
      pure $ Constr constr fields
    decodeMap = do
      obj <- decodeJson json
      map1 <- (obj .: "map" :: Either _ (Array _))
      kvs <- for map1 \entryJson -> do
        key <- entryJson .: "key"
        value <- entryJson .: "value"
        pure $ key /\ value
      pure $ Map (Map.fromFoldable kvs)
    decodeList = do
      List <$> decodeJson json
    decodeInteger = do
      integerStr <- decodeJson json
      case BigInt.fromString integerStr of
        Nothing -> Left $ UnexpectedValue json
        Just res -> pure $ Integer res
    decodeBytes = do
      bytesHex <- decodeJson json
      case hexToByteArray bytesHex of
        Nothing -> Left $ UnexpectedValue json
        Just res -> pure $ Bytes res
