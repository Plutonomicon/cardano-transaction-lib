module Types.JsonWsp where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), caseJsonArray, caseJsonNumber, caseJsonObject, caseJsonString, getField, decodeJson)
import Data.Array (index)
import Data.BigInt as BigInt
import Data.Either(Either(..), hush, note)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import Data.Foldable (foldl)
import Data.Map as Map
import Effect (Effect)
import Foreign.Object (Object)
import Types.Transaction (Value(..), CurrencySymbol(..), TokenName(..))

-- creates a unique id prefixed by its argument
foreign import _uniqueId :: String -> Effect String

-- denotes which query (Utxo, tx, datum, etc) we are making
data QueryType = UTXO

derive instance genericQueryType :: Generic QueryType _

instance showQueryType :: Show QueryType where
  show a = genericShow a 


--  the Address type in `Types.Transaction` is quite a bit more complex than 
--  this
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

-- this is fully determined by us - we can adjust this type as we have more complex
-- needs, it always just gets echoed back, so it is useful for req/res pairing
type Mirror = { step :: String, id :: String }

-- | make a well-formed Utxo Query with a unique ID attached
mkUtxosAtQuery :: UtxoQueryParams -> Effect (JsonWspRequest (QueryArgs UtxoQueryParams))
mkUtxosAtQuery uqp = mkJsonWspQuery uqp UTXO

-- this is polymorphic over the queryArgs even though QueryType should make them
-- concrete,  we could have some kind of lawless typeclass do this,
-- but here we've chosen to just provide concrete impls where we want to support it

-- once we add fixed export lists to this repo, this should NOT be exported
mkJsonWspQuery :: forall a. a -> QueryType -> Effect (JsonWspRequest (QueryArgs a))
mkJsonWspQuery a qt = do
  id <- _uniqueId (show qt <> "-") 
  pure  { type : "jsonwsp/request",
          version: "1.0",
          servicename: "ogmios",
          methodname: "Query",
          args: { query: a },
          mirror: { step: "INIT", id }
        }

-- the actual query description
type UtxoQueryParams = { utxo :: Array Address }

-- used as a wrapper for all Queries
type QueryArgs a = { query :: a }

-- convenient type for a UTXO query
type UtxoQueryBody = JsonWspRequest (QueryArgs UtxoQueryParams)

-- the response wrapper type for all websocket responses
type JsonWspResponse a = 
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , result :: a
  , reflection :: Mirror
}

-- polymorphic parser
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

-- parses json string at a given field to an ordinary string
parseFieldToString :: Object Json -> String -> Either JsonDecodeError String
parseFieldToString o str = 
  caseJsonString (Left (TypeMismatch ("expected field: '" <> str <> "' as a String"))) Right =<< getField o str

-- parses the number at the given field to a bigint
-- danger: Because argonaut has already parsed this as a Json foreign, it may
-- already be a javascript 'number' type. if that number is not safely representable
-- then we may have already lost precision. we may need a bigint preprocessor on json if this is the case: Because argonaut has already parsed this as a Json foreign, it may
-- already be a javascript 'number' type. if that number is not safely representable
-- then we may have already lost precision. we may need a bigint preprocessor on json if this is the case.
parseFieldToInt :: Object Json -> String -> Either JsonDecodeError BigInt.BigInt
parseFieldToInt o str = do
  let err = TypeMismatch $ "expected field: '" <> str <> "' as an Int"
  num <- caseJsonNumber (Left err) Right =<< getField o str
  int <- note err $ BigInt.fromNumber num
  pure int

-- parses a string at the given field to a BigInt
parseFieldToBigInt :: Object Json -> String -> Either JsonDecodeError BigInt.BigInt
parseFieldToBigInt o str = do
  let err = TypeMismatch $ "expected field: '" <> str <> "' as a BigInt"
  num <- caseJsonString (Left err) Right =<< getField o str
  bint <- note err $ BigInt.fromString num
  pure bint

-- parser for the `Mirror` type.
parseMirror :: Json -> Either JsonDecodeError Mirror
parseMirror = caseJsonObject (Left (TypeMismatch "expected object")) $
  (\o -> do
    step <- parseFieldToString o "step"
    id <- parseFieldToString o "id"
    pure { step, id }
  )

-- the outer result type for Utxo queries, newtyped so that it can have 
-- appropriate instances to work with `parseJsonWspResponse`
newtype UtxoQR = UtxoQR UtxoQueryResult

derive newtype instance showUtxoQR :: Show UtxoQR

instance decodeJsonUtxoQR :: DecodeJson UtxoQR where
  decodeJson j = UtxoQR <$> parseUtxoQueryResult j

-- the inner type for Utxo Queries
type UtxoQueryResult = Map.Map TxOutRef OgmiosTxOut 

-- TxOutRef
type TxOutRef = 
  { txId :: String,
    index :: BigInt.BigInt
  }

parseUtxoQueryResult :: Json -> Either JsonDecodeError UtxoQueryResult
parseUtxoQueryResult = caseJsonArray (Left (TypeMismatch "Expected Array")) $ 
  (\array -> foldl insertFunc (Right Map.empty) array )
  where
    insertFunc 
      :: Either JsonDecodeError UtxoQueryResult 
      -> Json 
      -> Either JsonDecodeError UtxoQueryResult 
    insertFunc acc = caseJsonArray (Left (TypeMismatch "Expected Array")) $ inner
      where
        inner :: Array Json -> Either JsonDecodeError UtxoQueryResult 
        inner innerArray = do
          txOutRefJson <- note (TypeMismatch "missing 0th element, expected a TxOutRef") $ 
            index innerArray 0
          txOutJson <- note (TypeMismatch "missing 1st element, expected a TxOut") $ 
            index innerArray 1
          txOutRef <- parseTxOutRef txOutRefJson
          txOut <- parseTxOut txOutJson 
          Map.insert txOutRef txOut <$> acc

-- helper for assuming we get an object
jsonObject 
  :: forall a
   . (Object Json -> Either JsonDecodeError a) 
  -> Json 
  -> Either JsonDecodeError a 
jsonObject = caseJsonObject (Left (TypeMismatch "expected object"))

-- parser for txOutRef
parseTxOutRef :: Json -> Either JsonDecodeError TxOutRef
parseTxOutRef = jsonObject $ 
  (\o -> do
    txId <- parseFieldToString o "txId"
    index <- parseFieldToInt o "index"
    pure { txId, index }
  )

-- this OgmiosTxOut doesn't seem to be in line with the 
-- `Types.Transaction.TransactionOutput` type,  we may need to reckon with this 
-- later.
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

-- parses the `Value` type
parseValue :: Object Json -> Either JsonDecodeError Value
parseValue outer = do
  o <- getField outer "value"
  coins <- parseFieldToBigInt o "coins" <|> (parseFieldToInt o "coins") <|> Left (TypeMismatch "Expected 'coins' to be an Int or a BigInt")
  (assetsJson :: {}) <- getField o "assets"
  -- note 'coins' is being sent as a number, in some cases this may exceed the max safe 
  -- representation of a Number, we may need to parse this up from a string instead of from 
  -- the Argonaut 'Json' representation in order to prevent this.
  -- there is probably a javascript library that has a custom parser we can use.

  -- assets are currently assumed to be empty
  -- newtype Value = Value (Map CurrencySymbol (Map TokenName BigInt.BigInt))
  pure $ Value $ Map.singleton (CurrencySymbol "") (Map.singleton (TokenName "") coins)
