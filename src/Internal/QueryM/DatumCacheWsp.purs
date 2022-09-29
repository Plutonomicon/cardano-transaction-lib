module Ctl.Internal.QueryM.DatumCacheWsp
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
import Control.Alt ((<|>))
import Ctl.Internal.Base64 (Base64String)
import Ctl.Internal.QueryM.JsonWsp (JsonWspCall, mkCallType)
import Ctl.Internal.QueryM.UniqueId (ListenerId)
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex)
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))

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

      decodeDatum
        :: Aeson -> Either JsonDecodeError (DataHash /\ Either String Datum)
      decodeDatum obj = caseAesonObject (Left $ TypeMismatch "Object")
        (\o -> (/\) <$> map wrap (o .: "hash") <*> decodeValueOption obj)
        obj

      decodeValueOption
        :: Aeson -> Either JsonDecodeError (Either String Datum)
      decodeValueOption aes =
        do
          options <- (pure <$> getNestedAeson aes [ "value", "Right" ])
            <|> (Left <$> getNestedAeson aes [ "value", "Left", "error" ])
          case options of
            Left x -> pure $ Left $ show x
            Right x -> pure <$> decodeAeson x

    in
      map GetDatumsByHashesR <<< decodeDatumArray =<< getNestedAeson
        r
        [ "value" ]

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

