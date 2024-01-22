module Cardano.Types.PlutusData
  ( PlutusData
      ( Constr
      , Map
      , List
      , Integer
      , Bytes
      )
  , pprintPlutusData
  , toCsl
  , fromCsl
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(UnexpectedValue)
  , decodeAeson
  , encodeAeson
  , toStringifiedNumbersJson
  , (.:)
  )
import Cardano.Serialization.Lib
  ( constrPlutusData_alternative
  , constrPlutusData_data
  , constrPlutusData_new
  , packMapContainer
  , plutusData_asBytes
  , plutusData_asConstrPlutusData
  , plutusData_asInteger
  , plutusData_asList
  , plutusData_asMap
  , plutusData_newBytes
  , plutusData_newConstrPlutusData
  , plutusData_newInteger
  , plutusData_newList
  , plutusData_newMap
  , unpackMapContainer
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Serialization.Lib.Internal
  ( packListContainer
  , unpackListContainer
  )
import Cardano.Types.BigInt (fromCsl, toCsl) as BigInt
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Control.Alt ((<|>))
import Data.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import JS.BigInt (BigInt)
import JS.BigInt (toString) as BigInt
import Partial.Unsafe (unsafePartial)

-- Doesn't distinguish "BuiltinData" and "Data", unlike Plutus:
data PlutusData
  = Constr BigNum (Array PlutusData)
  | Map (Array (Tuple PlutusData PlutusData))
  | List (Array PlutusData)
  | Integer BigInt
  | Bytes ByteArray

derive instance Eq PlutusData
derive instance Ord PlutusData
derive instance Generic PlutusData _

instance Show PlutusData where
  show x = genericShow x

-- Based off Ogmios Datum Cache Json format, although we no longer use ODC
instance DecodeAeson PlutusData where
  decodeAeson aeson = decodeConstr
    <|> decodeMap
    <|> decodeList
    <|> decodeInteger
    <|> decodeBytes
    where
    decodeConstr :: Either JsonDecodeError PlutusData
    decodeConstr = do
      x <- decodeAeson aeson
      constr <- x .: "constr"
      fields <- x .: "fields"
      pure $ Constr constr fields

    decodeMap :: Either JsonDecodeError PlutusData
    decodeMap = do
      obj <- decodeAeson aeson
      map1 <- (obj .: "map" :: Either _ (Array _))
      kvs <- for map1 \entryJson -> do
        key <- entryJson .: "key"
        value <- entryJson .: "value"
        pure $ key /\ value
      pure $ Map kvs

    decodeList :: Either JsonDecodeError PlutusData
    decodeList = do
      List <$> decodeAeson aeson

    decodeInteger :: Either JsonDecodeError PlutusData
    decodeInteger = do
      Integer <$> decodeAeson aeson

    decodeBytes :: Either JsonDecodeError PlutusData
    decodeBytes = do
      bytesHex <- decodeAeson aeson
      case hexToByteArray bytesHex of
        Nothing -> Left $ UnexpectedValue $ toStringifiedNumbersJson $
          encodeAeson bytesHex
        Just res -> pure $ Bytes res

instance EncodeAeson PlutusData where
  encodeAeson (Constr constr fields) = encodeAeson
    { "constr": constr
    , "fields": fields
    }
  encodeAeson (Map elems) = encodeAeson
    { "map": encodeAeson $ map
        ( \(k /\ v) ->
            { "key": k
            , "value": v
            }
        )
        elems
    }
  encodeAeson (List elems) = encodeAeson elems
  encodeAeson (Integer bi) = encodeAeson bi
  encodeAeson (Bytes ba) = encodeAeson ba

pprintPlutusData :: PlutusData -> TagSet
pprintPlutusData (Constr n children) = TagSet.fromArray
  [ ("Constr " <> BigInt.toString (BigNum.toBigInt n)) `tagSetTag`
      TagSet.fromArray (pprintPlutusData <$> children)
  ]
pprintPlutusData (Map entries) = TagSet.fromArray
  [ tagSetTag "Map" $ TagSet.fromArray $
      entries <#> \(key /\ value) ->
        TagSet.fromArray
          [ "key" `tagSetTag` pprintPlutusData key
          , "value" `tagSetTag` pprintPlutusData value
          ]
  ]
pprintPlutusData (List children) = TagSet.fromArray
  [ tagSetTag "List" $ TagSet.fromArray $
      children <#> pprintPlutusData
  ]
pprintPlutusData (Integer n) = TagSet.fromArray
  [ "Integer" `tag` BigInt.toString n ]
pprintPlutusData (Bytes bytes) = TagSet.fromArray
  [ "Bytes" `tag` byteArrayToHex bytes ]

-- serialization

toCsl :: PlutusData -> Csl.PlutusData
toCsl = case _ of
  Constr alt list -> convertConstr alt list
  Map mp -> convertPlutusMap mp
  List lst -> convertPlutusList lst
  Integer n -> convertPlutusInteger n
  Bytes b -> plutusData_newBytes b
  where
  convertConstr :: BigNum -> Array PlutusData -> Csl.PlutusData
  convertConstr alt list =
    plutusData_newConstrPlutusData $ constrPlutusData_new (unwrap alt)
      (packListContainer $ map toCsl list)

  convertPlutusList :: Array PlutusData -> Csl.PlutusData
  convertPlutusList pd =
    plutusData_newList <<< packListContainer $ map toCsl pd

  convertPlutusMap :: Array (PlutusData /\ PlutusData) -> Csl.PlutusData
  convertPlutusMap mp =
    plutusData_newMap $ packMapContainer $ map (toCsl *** toCsl) mp

  convertPlutusInteger :: BigInt -> Csl.PlutusData
  convertPlutusInteger =
    plutusData_newInteger <<< BigInt.toCsl

-- deserialization
fromCsl :: Csl.PlutusData -> PlutusData
fromCsl pd = unsafePartial $ fromJust $
  (convertPlutusConstr <$> toMaybe (plutusData_asConstrPlutusData pd))
    <|> (convertPlutusMap <$> toMaybe (plutusData_asMap pd))
    <|> (convertPlutusList <$> toMaybe (plutusData_asList pd))
    <|> (convertPlutusInteger <$> toMaybe (plutusData_asInteger pd))
    <|>
      (Bytes <$> toMaybe (plutusData_asBytes pd))
  where
  convertPlutusConstr :: Csl.ConstrPlutusData -> PlutusData
  convertPlutusConstr constr = do
    let
      datas = unpackListContainer $ constrPlutusData_data constr
      alt = constrPlutusData_alternative constr
    Constr (wrap alt) $ map fromCsl datas

  convertPlutusMap :: Csl.PlutusMap -> PlutusData
  convertPlutusMap pm =
    Map $ map (fromCsl *** fromCsl) $ unpackMapContainer pm

  convertPlutusList :: Csl.PlutusList -> PlutusData
  convertPlutusList = unpackListContainer >>> map fromCsl >>> List

  convertPlutusInteger :: Csl.BigInt -> PlutusData
  convertPlutusInteger = Integer <<< BigInt.fromCsl
