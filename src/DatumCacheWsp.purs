module DatumCacheWsp where

import Control.Alt ((<$), (<$>), (<|>))
import Control.Bind ((=<<))
import Control.Category ((<<<))
import Data.Argonaut (Json, JsonDecodeError(..), caseJsonObject, decodeJson, encodeJson, fromString, stringify)
import Data.Array (foldM)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Either (Either(..), note)
import Data.Function (const, ($))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Foreign.Object (Object)
import Foreign.Object as FO
import Types.ByteArray (byteArrayToHex)
import Types.PlutusData (PlutusData, DatumHash)


newtype BlockId = BlockId BigInt -- an integer on HS side

newtype WspFault = WspFault Json

faultToString :: WspFault -> String
faultToString (WspFault j) = stringify j

type JsonWspRequest =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , args :: Json
  }

type JsonWspResponse =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , result :: Maybe Json
  , fault :: Maybe Json
  }

data DatumCacheRequest
  = GetDatumByHashRequest DatumHash
  | GetDatumsByHashesRequest (Array DatumHash)

data DatumCacheResponse
  = GetDatumByHashResponse (Maybe PlutusData)
  | GetDatumsByHashesResponse (Array PlutusData)


requestMethodName :: DatumCacheRequest -> String
requestMethodName = case _ of
  GetDatumByHashRequest _ -> "GetDatumByHash"
  GetDatumsByHashesRequest _ -> "GetDatumsByHashes"


jsonWspRequest :: DatumCacheRequest -> JsonWspRequest
jsonWspRequest req =
  { type: "jsonwsp/request"
  , version: "1.0"
  , servicename: "ogmios"
  , methodname: requestMethodName req
  , args: toArgs req
  }
  where
    toArgs :: DatumCacheRequest -> Json
    toArgs = case _ of
      GetDatumByHashRequest dh -> encodeJson {hash: byteArrayToHex $ unwrap dh}
      GetDatumsByHashesRequest dhs -> encodeJson {hashes: (byteArrayToHex <<< unwrap) <$> dhs}


parseJsonWspResponse :: JsonWspResponse -> Either WspFault DatumCacheResponse
parseJsonWspResponse resp@{methodname, result, fault} =
  maybe
  (Left $ maybe invalidResponseError WspFault fault)
  decodeResponse
  result
  where
    decodeResponse r = case methodname of
      "GetDatumByHash" -> GetDatumByHashResponse <$>
        let
          datumFound =
            Just <$> liftErr (decodeJson =<< deepLookup ["DatumFound", "value"] r)
          datumNotFound =
            Nothing <$ liftErr (deepLookup ["DatumNotFound"] r)
        in datumFound <|> datumNotFound

      "GetDatumsByHashes" -> GetDatumsByHashesResponse <$>
          liftErr (decodeJson =<< deepLookup ["DatumFound", "value"] r)

      _ -> Left invalidResponseError

    liftErr :: forall a. Either JsonDecodeError a -> Either WspFault a
    liftErr = lmap (const invalidResponseError)

    invalidResponseError :: WspFault
    invalidResponseError = WspFault $ encodeJson
      { error: "Invalid datum cache response"
      , response: resp}


-- helper for assuming we get an object
jsonObject
  :: forall a
   . (Object Json -> Either WspFault a)
  -> Json
  -> Either WspFault a
jsonObject = caseJsonObject (Left (WspFault $ fromString "expected object"))



deepLookup :: Array String -> Json -> Either JsonDecodeError Json
deepLookup keys obj = note (UnexpectedValue obj) $ foldM lookup obj keys
  where
    lookup :: Json -> String -> Maybe Json
    lookup j lbl = caseJsonObject Nothing (FO.lookup lbl) j


-- TODO
-- startFetchBlocks :: Slot -> BlockId -> M (Response Unit)
-- cancelFetchBlocks ::  M (Response Unit)
-- datumFilterAddHashes ::  Array DatumHash -> M (Response Unit)
-- datumFilterRemoveHashes ::  Array DatumHash -> M (Response Unit)
-- datumFilterSetHashes ::  Array DatumHash -> M (Response Unit)
-- datumFilterGetHashes ::  M (Response (Array DatumHash))
