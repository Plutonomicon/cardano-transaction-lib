module QueryM.DatumCacheWsp
  ( DatumCacheResponse(..)
  , DatumCacheRequest(..)
  , DatumCacheMethod(..)
  , JsonWspRequest
  , JsonWspResponse
  , WspFault(WspFault)
  , faultToString
  , jsonWspRequest
  , parseJsonWspResponse
  , responseMethod
  , requestMethodName
  ) where

import Prelude
import Undefined

import Aeson
  ( Aeson
  , (.:)
  , caseAesonArray
  , caseAesonObject
  , decodeAeson
  , getNestedAeson
  , toStringifiedNumbersJson
  )
import Control.Alt ((<|>))
import Data.Argonaut
  ( Json
  , JsonDecodeError
      ( AtIndex
      , Named
      , TypeMismatch
      , UnexpectedValue
      )
  , decodeJson
  , encodeJson
  , jsonNull
  , stringify
  )
import Data.Bifunctor (lmap)
import Data.Either (Either(Left), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Serialization.Address (BlockId, Slot)
import Types.ByteArray (byteArrayToHex, hexToByteArray)
import Types.Datum (Datum, DatumHash)
import Types.Transaction (DataHash(DataHash))

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
  , result :: Maybe Aeson
  , fault :: Maybe Aeson
  }

data DatumCacheMethod
  = GetDatumByHash
  | GetDatumsByHashes
  | StartFetchBlocks
  | CancelFetchBlocks
  | DatumFilterAddHashes
  | DatumFilterRemoveHashes
  | DatumFilterSetHashes
  | DatumFilterGetHashes

derive instance Eq DatumCacheMethod

instance Show DatumCacheMethod where
  show = datumCacheMethodToString

datumCacheMethodToString :: DatumCacheMethod -> String
datumCacheMethodToString = case _ of
  GetDatumByHash -> "GetDatumByHash"
  GetDatumsByHashes -> "GetDatumsByHashes"
  StartFetchBlocks -> "StartFetchBlocks"
  CancelFetchBlocks -> "CancelFetchBlocks"
  DatumFilterAddHashes -> "DatumFilterAddHashes"
  DatumFilterRemoveHashes -> "DatumFilterRemoveHashes"
  DatumFilterSetHashes -> "DatumFilterSetHashes"
  DatumFilterGetHashes -> "DatumFilterGetHashes"

datumCacheMethodFromString :: String -> Maybe DatumCacheMethod
datumCacheMethodFromString = case _ of
  "GetDatumByHash" -> Just GetDatumByHash
  "GetDatumsByHashes" -> Just GetDatumsByHashes
  "StartFetchBlocks" -> Just StartFetchBlocks
  "CancelFetchBlocks" -> Just CancelFetchBlocks
  "DatumFilterAddHashes" -> Just DatumFilterAddHashes
  "DatumFilterRemoveHashes" -> Just DatumFilterRemoveHashes
  "DatumFilterSetHashes" -> Just DatumFilterSetHashes
  "DatumFilterGetHashes" -> Just DatumFilterGetHashes
  _ -> Nothing

data DatumCacheRequest
  = GetDatumByHashRequest DatumHash
  | GetDatumsByHashesRequest (Array DatumHash)
  | StartFetchBlocksRequest { slot :: Slot, id :: BlockId }
  | CancelFetchBlocksRequest
  | DatumFilterAddHashesRequest (Array DatumHash)
  | DatumFilterRemoveHashesRequest (Array DatumHash)
  | DatumFilterSetHashesRequest (Array DatumHash)
  | DatumFilterGetHashesRequest

data DatumCacheResponse
  = GetDatumByHashResponse (Maybe Datum)
  | GetDatumsByHashesResponse (Map DatumHash Datum)
  | StartFetchBlocksResponse
  | CancelFetchBlocksResponse
  | DatumFilterAddHashesResponse
  | DatumFilterRemoveHashesResponse
  | DatumFilterSetHashesResponse
  | DatumFilterGetHashesResponse (Array DatumHash)

requestMethod :: DatumCacheRequest -> DatumCacheMethod
requestMethod = case _ of
  GetDatumByHashRequest _ -> GetDatumByHash
  GetDatumsByHashesRequest _ -> GetDatumsByHashes
  StartFetchBlocksRequest _ -> StartFetchBlocks
  CancelFetchBlocksRequest -> CancelFetchBlocks
  DatumFilterAddHashesRequest _ -> DatumFilterAddHashes
  DatumFilterRemoveHashesRequest _ -> DatumFilterRemoveHashes
  DatumFilterSetHashesRequest _ -> DatumFilterSetHashes
  DatumFilterGetHashesRequest -> DatumFilterGetHashes

responseMethod :: DatumCacheResponse -> DatumCacheMethod
responseMethod = case _ of
  GetDatumByHashResponse _ -> GetDatumByHash
  GetDatumsByHashesResponse _ -> GetDatumsByHashes
  StartFetchBlocksResponse -> StartFetchBlocks
  CancelFetchBlocksResponse -> CancelFetchBlocks
  DatumFilterAddHashesResponse -> DatumFilterAddHashes
  DatumFilterRemoveHashesResponse -> DatumFilterRemoveHashes
  DatumFilterSetHashesResponse -> DatumFilterSetHashes
  DatumFilterGetHashesResponse _ -> DatumFilterGetHashes

requestMethodName :: DatumCacheRequest -> String
requestMethodName = requestMethod >>> datumCacheMethodToString

jsonWspRequest :: DatumCacheRequest -> JsonWspRequest
jsonWspRequest req =
  { type: "jsonwsp/request"
  , version: "1.0"
  , servicename: "ogmios"
  , methodname: requestMethodName req
  , args: toArgs req
  }
  where
  encodeHashes :: Array DatumHash -> Json
  encodeHashes dhs = encodeJson { hashes: (byteArrayToHex <<< unwrap) <$> dhs }

  toArgs :: DatumCacheRequest -> Json
  toArgs = case _ of
    GetDatumByHashRequest dh -> encodeJson { hash: byteArrayToHex $ unwrap dh }
    GetDatumsByHashesRequest dhs -> encodeHashes dhs
    StartFetchBlocksRequest slotnblock -> encodeJson slotnblock
    CancelFetchBlocksRequest -> jsonNull
    DatumFilterAddHashesRequest dhs -> encodeHashes dhs
    DatumFilterRemoveHashesRequest dhs -> encodeHashes dhs
    DatumFilterSetHashesRequest dhs -> encodeHashes dhs
    DatumFilterGetHashesRequest -> jsonNull

parseJsonWspResponse :: JsonWspResponse -> Either WspFault DatumCacheResponse
parseJsonWspResponse resp@{ methodname, result, fault } =
  maybe
    (toLeftWspFault fault)
    decodeResponse
    result
  where
  toLeftWspFault :: Maybe Aeson -> Either WspFault DatumCacheResponse
  toLeftWspFault = Left <<< maybe invalidResponseError WspFault <<< map
    toStringifiedNumbersJson

  decodeResponse :: Aeson -> Either WspFault DatumCacheResponse
  decodeResponse r = case datumCacheMethodFromString methodname of
    Nothing -> Left invalidResponseError
    Just method -> case method of
      GetDatumByHash -> GetDatumByHashResponse <$>
        let
          datumFound :: Either WspFault (Maybe Datum)
          datumFound =
            Just <$> liftErr
              (decodeAeson =<< getNestedAeson r [ "DatumFound", "value" ])

          datumNotFound :: Either WspFault (Maybe Datum)
          datumNotFound =
            Nothing <$ liftErr (getNestedAeson r [ "DatumNotFound" ])
        in
          datumFound <|> datumNotFound
      GetDatumsByHashes -> liftErr $
        let
          decodeDatumArray
            :: Aeson -> Either JsonDecodeError (Map DatumHash Datum)
          decodeDatumArray =
            caseAesonArray (Left $ TypeMismatch "expected array")
              $ (map Map.fromFoldable) <<< traverse decodeDatum

          decodeDatum
            :: Aeson -> Either JsonDecodeError (DatumHash /\ Datum)
          decodeDatum = caseAesonObject (Left $ TypeMismatch "expected object")
            $ \o -> (/\) <$> map wrap (o .: "hash") <*>
                (decodeAeson =<< o .: "value")
        in
          map GetDatumsByHashesResponse <<< decodeDatumArray =<< getNestedAeson
            r
            [ "DatumsFound", "value" ]

      StartFetchBlocks -> StartFetchBlocksResponse <$ decodeDoneFlag
        [ "StartedBlockFetcher" ]
        r
      -- fault version of the response should probably be implemented as one of
      -- expected results of API call
      CancelFetchBlocks -> CancelFetchBlocksResponse <$ decodeDoneFlag
        [ "StoppedBlockFetcher" ]
        r
      DatumFilterAddHashes -> DatumFilterAddHashesResponse <$ decodeDoneFlag
        [ "AddedHashes" ]
        r
      DatumFilterRemoveHashes -> DatumFilterRemoveHashesResponse <$
        decodeDoneFlag
          [ "RemovedHashes" ]
          r
      DatumFilterSetHashes -> DatumFilterSetHashesResponse <$ decodeDoneFlag
        [ "SetHashes" ]
        r
      DatumFilterGetHashes -> DatumFilterGetHashesResponse <$>
        liftErr (decodeHashes =<< getNestedAeson r [ "hashes" ])

  decodeHashes :: Aeson -> Either JsonDecodeError (Array DatumHash)
  decodeHashes j = do
    { hashes } :: { hashes :: Array String } <- decodeJson jstr
    forWithIndex hashes $ \idx h ->
      note
        ( AtIndex idx $ Named ("Cannot convert to ByteArray: " <> h) $
            UnexpectedValue jstr
        )
        $ DataHash <$> hexToByteArray h
    where
    jstr :: Json
    jstr = toStringifiedNumbersJson j

  invalidResponseError :: WspFault
  invalidResponseError = WspFault $ encodeJson
    { error: "Invalid datum cache response"
    , response: resp
        { result = toStringifiedNumbersJson <$> resp.result
        , fault = toStringifiedNumbersJson <$> resp.fault
        }
    }

  liftErr :: forall (a :: Type). Either JsonDecodeError a -> Either WspFault a
  liftErr = lmap $ const invalidResponseError

  decodeDoneFlag :: Array String -> Aeson -> Either WspFault Unit
  decodeDoneFlag locator r =
    unlessM (liftErr (decodeAeson =<< getNestedAeson r locator))
      $ Left invalidResponseError
