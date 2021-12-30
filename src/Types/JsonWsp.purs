module Types.JsonWsp where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), caseJsonArray, caseJsonNumber, caseJsonObject, caseJsonString, getField, decodeJson)
import Data.Array (index)
import Data.BigInt as BigInt
import Data.Either(Either(..), hush, note)
import Data.Maybe (Maybe)
import Data.Int53 as Int
import Data.Foldable (foldl)
import Data.Map as Map
import Foreign.Object (Object)
import Types.Transaction (Value(..), CurrencySymbol(..), TokenName(..))

import Debug.Trace (traceM, spy)

type Address = String
-- these types are described in: https://ogmios.dev/getting-started/basics/ 
type JsonWspRequest a =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , args :: a
  , mirror :: Mirror
  }

type Mirror = { step :: String }

parseMirror :: Json -> Either JsonDecodeError Mirror
parseMirror = caseJsonObject (Left (TypeMismatch "expected object")) $
  (\o -> do
    step <- parseFieldToString o "step"
    pure { step }
  )

mkJsonWspQuery :: forall a. a -> JsonWspRequest (QueryArgs a)
mkJsonWspQuery a = 
  { type : "jsonwsp/request",
    version: "1.0",
    servicename: "ogmios",
    methodname: "Query",
    args: { query: a },
    mirror: { step: "INIT" }
  }

type UtxoQueryParams = { utxo :: Array Address }
type QueryArgs a = { query :: a }
type UtxoQueryBody = JsonWspRequest (QueryArgs UtxoQueryParams)

type JsonWspResponse a = 
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , result :: a
  , reflection :: Mirror
}

parseFieldToString :: Object Json -> String -> Either JsonDecodeError String
parseFieldToString o str = 
  caseJsonString (Left (TypeMismatch ("expected field: '" <> str <> "' as a String"))) Right =<< getField o str

parseFieldToInt :: Object Json -> String -> Either JsonDecodeError Int.Int53
parseFieldToInt o str = do
  let err = TypeMismatch $ "expected field: '" <> str <> "' as an Int"
  num <- caseJsonNumber (Left err) Right =<< getField o str
  int <- note err $ Int.fromNumber num
  pure int

parseFieldToBigInt :: Object Json -> String -> Either JsonDecodeError BigInt.BigInt
parseFieldToBigInt o str = do
  let err = TypeMismatch $ "expected field: '" <> str <> "' as a BigInt"
  num <- caseJsonString (Left err) Right =<< getField o str
  bint <- note err $ BigInt.fromString num
  pure bint

parseJsonWspResponse 
  :: forall a
   . DecodeJson a 
  => Json 
  -> Either JsonDecodeError (JsonWspResponse a)
parseJsonWspResponse = jsonObject
  (\o -> do
    typeField <- parseFieldToString o "type"
    version <- parseFieldToString o "version"
    servicename <- parseFieldToString o "servicename"
    methodname <- parseFieldToString o "methodname"
    result <- decodeJson =<< getField o "result"
    reflection <- parseMirror =<< getField o "reflection"
    pure { "type": typeField, version, servicename, methodname, result, reflection }
  )

-- Utxo Query result:
-- it appears that the outer array is `Array Utxo` or similar (see `Types.Transaction`) to match up
-- the inner array appears to be an ADT serialized without keys. I suspect we'll want to make this fit one of the inherited types from cardano-serialization-lib if we can as these will be used for balancing.
-- it seems that the two types used here roughtly correspond to a `TransactionInput` and a `TransactionOutput`
-- or perhaps a txOut and a TxOutRef?
-- since we're trying to replicate utxosAt - it looks like we had better return 
-- a `(Map TxOutRef ChainIndexTxOut)` where a `ChainIndexTxOut` has different constructors for txOuts held by a wallet address vs. a validator hash
-- in all likelihood we will need to add additional parsing as we address additional usecases.
-- [ 
  -- [
    -- { "txId":"1ae7bbe9ed3d0205b3704446678c3a9e777ba41a65a3115d066dd4eabe295839",
      -- "index":0
      -- },
    -- {"address":"addr_test1qr7g8nrv76fc7k4ueqwecljxx9jfwvsgawhl55hck3n8uwaz26mpcwu58zdkhpdnc6nuq3fa8vylc8ak9qvns7r2dsysp7ll4d",
    -- "value": {"coins":1000000000,"assets":{}},
    -- "datum": null
    -- }
  -- ]
-- ]

newtype UtxoQR = UtxoQR UtxoQueryResult

instance decodeJsonUtxoQR :: DecodeJson UtxoQR where
  decodeJson j = UtxoQR <$> parseUtxoQueryResult j

type UtxoQueryResult = Map.Map TxOutRef OgmiosTxOut 

type TxOutRef = 
  { txId :: String,
    index :: Int.Int53
  }

parseUtxoQueryResult :: Json -> Either JsonDecodeError UtxoQueryResult
parseUtxoQueryResult = caseJsonArray (Left (TypeMismatch "Expected Array")) $ 
  (\array -> foldl insertFunc (Right Map.empty) array )

insertFunc 
  :: Either JsonDecodeError UtxoQueryResult 
  -> Json 
  -> Either JsonDecodeError UtxoQueryResult 
insertFunc acc = caseJsonArray (Left (TypeMismatch "Expected Array")) $ inner
  where
    inner :: Array Json -> Either JsonDecodeError UtxoQueryResult 
    inner innerArray = do
      txOutRefJson <- note (TypeMismatch "missing 0th element") $ index innerArray 0
      txOutJson <- note (TypeMismatch "missing 1st element") $ index innerArray 1
      txOutRef <- parseTxOutRef txOutRefJson
      txOut <- parseTxOut txOutJson 
      Map.insert txOutRef txOut <$> acc

jsonObject :: forall a. (Object Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError a 
jsonObject = caseJsonObject (Left (TypeMismatch "expected object"))

parseTxOutRef :: Json -> Either JsonDecodeError TxOutRef
parseTxOutRef = jsonObject $ 
  (\o -> do
    txId <- parseFieldToString o "txId"
    index <- parseFieldToInt o "index"
    pure { txId, index }
  )

-- [ 
  -- [
    -- { "txId":"1ae7bbe9ed3d0205b3704446678c3a9e777ba41a65a3115d066dd4eabe295839",
      -- "index":0
      -- },
    -- {"address":"addr_test1qr7g8nrv76fc7k4ueqwecljxx9jfwvsgawhl55hck3n8uwaz26mpcwu58zdkhpdnc6nuq3fa8vylc8ak9qvns7r2dsysp7ll4d",
    -- "value": {"coins":1000000000,"assets":{}},
    -- "datum": null
    -- }
  -- ]
-- ]

type OgmiosTxOut = 
  { address :: String, 
    value :: Value, 
    datum :: Maybe String 
  }

-- Ogmios currently supplies the Raw Address in addr1 format, rather than the 
-- cardano-serialization-lib 'Address' type,  perhaps this information can be 
-- extracted.
parseTxOut :: Json -> Either JsonDecodeError OgmiosTxOut  
parseTxOut = jsonObject $
  (\o -> do
    address <- parseFieldToString o "address"
    value <- parseValue o
    let datum = hush $ parseFieldToString o "address" 
    pure $ { address, value, datum }
  )

parseValue :: Object Json -> Either JsonDecodeError Value
parseValue outer = do
  o <- getField outer "value"
  traceM "trying to parse coins"
  _ <- pure $ spy "object: " o
  coins <- parseFieldToBigInt o "coins" <|> (convertIntParsing $ parseFieldToInt o "coins") <|> Left (TypeMismatch "Expected 'coins' to be an Int or a BigInt")
  (assetsJson :: {}) <- getField o "assets"
  -- assets are currently assumed to be empty
  -- newtype Value = Value (Map CurrencySymbol (Map TokenName BigInt.BigInt))
  pure $ Value $ Map.singleton (CurrencySymbol "") (Map.singleton (TokenName "") coins)
convertIntParsing 
  :: Either JsonDecodeError Int.Int53 
  -> Either JsonDecodeError BigInt.BigInt
convertIntParsing (Left e) = Left e
convertIntParsing (Right i) = do
   note (TypeMismatch "unexpected conversion failure from Int to BigInt") $ BigInt.fromString $ Int.toString i
