module QueryM.DatumCacheWsp
  ( DatumCacheMethod
      ( GetDatumByHash
      , GetDatumsByHashes
      , StartFetchBlocks
      , CancelFetchBlocks
      )
  , GetDatumByHashR(GetDatumByHashR)
  , GetDatumsByHashesR(GetDatumsByHashesR)
  , StartFetchBlocksR(StartFetchBlocksR)
  , CancelFetchBlocksR(CancelFetchBlocksR)
  , JsonWspRequest
  , JsonWspResponse
  , WspFault(WspFault)
  , faultToString
  , getDatumByHashCall
  , getDatumsByHashesCall
  , startFetchBlocksCall
  , cancelFetchBlocksCall
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError
      ( TypeMismatch
      )
  , caseAesonArray
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getNestedAeson
  , stringifyAeson
  , (.:)
  )
import Control.Alt ((<|>))
import Data.Either (Either(Left))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import QueryM.JsonWsp (JsonWspCall, mkCallType)
import QueryM.UniqueId (ListenerId)
import Serialization.Address (Slot)
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray (byteArrayToHex)
import Types.Datum (Datum, DataHash)
import Types.Chain (BlockHeaderHash)

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

newtype GetDatumsByHashesR = GetDatumsByHashesR (Map DataHash Datum)

derive instance Newtype GetDatumsByHashesR _

instance DecodeAeson GetDatumsByHashesR where
  decodeAeson r =
    let
      decodeDatumArray
        :: Aeson -> Either JsonDecodeError (Map DataHash Datum)
      decodeDatumArray =
        caseAesonArray (Left $ TypeMismatch "expected array")
          $ (map Map.fromFoldable) <<< traverse decodeDatum

      decodeDatum
        :: Aeson -> Either JsonDecodeError (DataHash /\ Datum)
      decodeDatum = caseAesonObject (Left $ TypeMismatch "expected object")
        $ \o -> (/\) <$> map wrap (o .: "hash") <*>
            (decodeAeson =<< o .: "value")
    in
      map GetDatumsByHashesR <<< decodeDatumArray =<< getNestedAeson
        r
        [ "DatumsFound", "value" ]

data StartFetchBlocksR = StartFetchBlocksR

instance DecodeAeson StartFetchBlocksR where
  decodeAeson r = StartFetchBlocksR <$ decodeDoneFlag [ "StartedBlockFetcher" ]
    r

data CancelFetchBlocksR = CancelFetchBlocksR

instance DecodeAeson CancelFetchBlocksR where
  decodeAeson r = CancelFetchBlocksR <$ decodeDoneFlag
    [ "StoppedBlockFetcher" ]
    r

decodeDoneFlag :: Array String -> Aeson -> Either JsonDecodeError Unit
decodeDoneFlag locator r =
  unlessM ((decodeAeson =<< getNestedAeson r locator))
    $ Left (TypeMismatch "Expected done flag")

-- TODO: delete
data DatumCacheMethod
  = GetDatumByHash
  | GetDatumsByHashes
  | StartFetchBlocks
  | CancelFetchBlocks

derive instance Eq DatumCacheMethod

instance Show DatumCacheMethod where
  show = datumCacheMethodToString

datumCacheMethodToString :: DatumCacheMethod -> String
datumCacheMethodToString = case _ of
  GetDatumByHash -> "GetDatumByHash"
  GetDatumsByHashes -> "GetDatumsByHashes"
  StartFetchBlocks -> "StartFetchBlocks"
  CancelFetchBlocks -> "CancelFetchBlocks"

getDatumByHashCall :: JsonWspCall DataHash GetDatumByHashR
getDatumByHashCall = mkDatumCacheCallType
  GetDatumByHash
  ({ hash: _ } <<< byteArrayToHex <<< unwrap)

getDatumsByHashesCall :: JsonWspCall (Array DataHash) GetDatumsByHashesR
getDatumsByHashesCall = mkDatumCacheCallType
  GetDatumsByHashes
  ({ hashes: _ } <<< map (byteArrayToHex <<< unwrap))

startFetchBlocksCall
  :: JsonWspCall { slot :: Slot, id :: BlockHeaderHash } StartFetchBlocksR
startFetchBlocksCall = mkDatumCacheCallType
  StartFetchBlocks
  ( \({ slot, id }) ->
      { slot
      , id: encodeAeson (unwrap id)
      , datumFilter: { "const": true }
      }
  )

cancelFetchBlocksCall :: JsonWspCall Unit CancelFetchBlocksR
cancelFetchBlocksCall = mkDatumCacheCallType
  CancelFetchBlocks
  (const {})

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
