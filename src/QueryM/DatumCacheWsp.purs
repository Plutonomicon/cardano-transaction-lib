module QueryM.DatumCacheWsp
  ( DatumCacheMethod
      ( GetDatumByHash
      , GetDatumsByHashes
      , GetTxByHash
      )
  , GetDatumByHashR(GetDatumByHashR)
  , GetDatumsByHashesR(GetDatumsByHashesR)
  , GetTxByHashR(GetTxByHashR)
  , WspFault(WspFault)
  , faultToString
  , getDatumByHashCall
  , getDatumsByHashesCall
  , getTxByHash
  , JsonWspRequest
  , JsonWspResponse
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonArray
  , caseAesonObject
  , decodeAeson
  , getNestedAeson
  , stringifyAeson
  , (.:)
  )
import Base64 (Base64String)
import Control.Alt ((<|>))
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import QueryM.JsonWsp (JsonWspCall, mkCallType)
import QueryM.UniqueId (ListenerId)
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray (ByteArray, byteArrayToHex)
import Types.Datum (Datum, DataHash)

newtype WspFault = WspFault Aeson

faultToString :: WspFault -> String
faultToString (WspFault j) = stringifyAeson j

type JsonWspRequest (a :: Type) =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , args :: a
  , mirror :: ListenerId
  }

type JsonWspResponse =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , result :: Maybe Aeson
  , fault :: Maybe WspFault
  , reflection :: ListenerId
  }

newtype GetDatumByHashR = GetDatumByHashR (Maybe Datum)

derive instance Newtype GetDatumByHashR _
derive instance Generic GetDatumByHashR _

instance Show GetDatumByHashR where
  show = genericShow

instance DecodeAeson GetDatumByHashR where
  decodeAeson r = GetDatumByHashR <$>
    let
      datumFound :: Either JsonDecodeError (Maybe Datum)
      datumFound =
        Just <$> (decodeAeson =<< getNestedAeson r [ "DatumFound", "value" ])

      datumNotFound :: Either JsonDecodeError (Maybe Datum)
      datumNotFound =
        Nothing <$ getNestedAeson r [ "DatumNotFound" ]
    in
      datumFound <|> datumNotFound

newtype GetDatumsByHashesR = GetDatumsByHashesR
  (Map DataHash (Either String Datum))

derive instance Newtype GetDatumsByHashesR _
derive instance Generic GetDatumsByHashesR _

instance Show GetDatumsByHashesR where
  show = genericShow

instance DecodeAeson GetDatumsByHashesR where
  decodeAeson r =
    let
      decodeDatumArray
        :: Aeson -> Either JsonDecodeError (Map DataHash (Either String Datum))
      decodeDatumArray =
        caseAesonArray (Left $ TypeMismatch "Array")
          $ (map Map.fromFoldable) <<< traverse decodeDatum

      decodeEmptyDatums obj =
        pure Map.empty *>
          ( caseAesonArray (Left $ TypeMismatch "Array")
              $ identity <<< traverse decodeEmptyDatum
          ) obj

      decodeEmptyDatum
        :: Aeson -> Either JsonDecodeError Unit
      decodeEmptyDatum aes =
        if stringifyAeson aes == "{}" then
          pure unit
        else
          Left $ TypeMismatch "expected empty JSON"

      decodeDatum
        :: Aeson -> Either JsonDecodeError (DataHash /\ Either String Datum)
      decodeDatum obj = caseAesonObject (Left $ TypeMismatch "Object")
        (\o -> (/\) <$> map wrap (o .: "hash") <*> (decodeValueOption obj))
        obj

      decodeValueOption
        :: Aeson -> Either JsonDecodeError (Either String Datum)
      decodeValueOption aes =
        do
          options <- (pure <$> getNestedAeson aes [ "value", "Right" ])
            <|> (Left <$> getNestedAeson aes [ "value", "Left" ])
          case options of
            Left x -> pure $ Left $ show x
            Right x -> pure <$> decodeAeson x

      datumsFound =
        map GetDatumsByHashesR <<< decodeDatumArray =<< getNestedAeson
          r
          [ "DatumsFound", "value" ]

      datumsEmpty =
        ( decodeEmptyDatums =<< getNestedAeson
            r
            [ "DatumsFound", "value" ]
        ) $> GetDatumsByHashesR Map.empty

      datumsNotFound =
        getNestedAeson r [ "DatumsNotFound" ] $> GetDatumsByHashesR Map.empty
    in
      datumsFound <|> datumsEmpty <|> datumsNotFound

-- TODO
-- This should be changed to `GetTxByHashR Transaction` once we support `getTxById`
--
-- See https://github.com/Plutonomicon/cardano-transaction-lib/issues/30
newtype GetTxByHashR = GetTxByHashR (Maybe Base64String)

derive instance Newtype GetTxByHashR _
derive instance Generic GetTxByHashR _

instance Show GetTxByHashR where
  show = genericShow

instance DecodeAeson GetTxByHashR where
  decodeAeson r = GetTxByHashR <$>
    let
      txFound :: Either JsonDecodeError (Maybe Base64String)
      txFound =
        getNestedAeson r [ "TxFound", "value" ] >>= decodeAeson

      txNotFound :: Either JsonDecodeError (Maybe Base64String)
      txNotFound =
        Nothing <$ getNestedAeson r [ "TxNotFound" ]
    in
      txFound <|> txNotFound

-- TODO: delete
data DatumCacheMethod
  = GetDatumByHash
  | GetDatumsByHashes
  | GetTxByHash

derive instance Eq DatumCacheMethod

instance Show DatumCacheMethod where
  show = datumCacheMethodToString

datumCacheMethodToString :: DatumCacheMethod -> String
datumCacheMethodToString = case _ of
  GetDatumByHash -> "GetDatumByHash"
  GetDatumsByHashes -> "GetDatumsByHashes"
  GetTxByHash -> "GetTxByHash"

getDatumByHashCall :: JsonWspCall DataHash GetDatumByHashR
getDatumByHashCall = mkDatumCacheCallType
  GetDatumByHash
  ({ hash: _ } <<< byteArrayToHex <<< unwrap)

getDatumsByHashesCall :: JsonWspCall (Array DataHash) GetDatumsByHashesR
getDatumsByHashesCall = mkDatumCacheCallType
  GetDatumsByHashes
  ({ hashes: _ } <<< map (byteArrayToHex <<< unwrap))

type TxHash = ByteArray

getTxByHash :: JsonWspCall TxHash GetTxByHashR
getTxByHash = mkDatumCacheCallType
  GetTxByHash
  ({ hash: _ } <<< byteArrayToHex)

-- convenience helper
mkDatumCacheCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonWspRequest a)
  => DatumCacheMethod
  -> (i -> a)
  -> JsonWspCall i o
mkDatumCacheCallType method args = mkCallType
  { "type": "jsonwsp/request"
  , version: "1.0"
  , servicename: "ogmios"
  }
  { methodname: datumCacheMethodToString method, args }
  Proxy
