module Types.JsonWsp
  ( OgmiosAddress
  , JsonWspResponse
  , Mirror
  , OgmiosTxOut
  , OgmiosTxOutRef
  , UtxoQR(UtxoQR)
  , OgmiosBlockHeaderHash(OgmiosBlockHeaderHash)
  , ChainOrigin(ChainOrigin)
  , ChainPoint
  , ChainTipQR
  , UtxoQueryResult
  , mkChainTipQuery
  , mkUtxosAtQuery
  , parseJsonWspResponse
  , parseFieldToString
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , caseAesonArray
  , caseAesonObject
  , caseAesonString
  , caseAesonUInt
  , caseAesonBigInt
  , decodeAeson
  , getField
  , getFieldOptional
  )
import Control.Alt ((<|>))
import Data.Argonaut (JsonDecodeError(TypeMismatch))
import Data.Array (index)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either
  ( Either(Left, Right)
  , hush
  , note
  )
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)
import Data.String
  ( Pattern(Pattern)
  , indexOf
  , splitAt
  , uncons
  )
import Serialization.Address (Slot)
import Data.Traversable (sequence)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.UInt as UInt
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as FO
import Types.ByteArray (hexToByteArray)
import Types.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , mkCurrencySymbol
  , mkTokenName
  , mkValue
  )
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (type (|+|), toEither1)

-- creates a unique id prefixed by its argument
foreign import _uniqueId :: String -> Effect String

-- denotes which query (Utxo, tx, datum, etc) we are making
data QueryType = UTXO | GetChainTip

derive instance genericQueryType :: Generic QueryType _

instance showQueryType :: Show QueryType where
  show a = genericShow a

--  the Address type in `Types.Transaction` is quite a bit more complex than
--  this
type OgmiosAddress = String

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

-- | make a well-formed Chain Tip Query with a unique ID attached
mkChainTipQuery :: Effect (JsonWspRequest (QueryArgs String))
mkChainTipQuery = mkJsonWspQuery "chainTip" GetChainTip

-- | make a well-formed Utxo Query with a unique ID attached
mkUtxosAtQuery
  :: UtxoQueryParams -> Effect (JsonWspRequest (QueryArgs UtxoQueryParams))
mkUtxosAtQuery uqp = mkJsonWspQuery uqp UTXO

-- this is polymorphic over the queryArgs even though QueryType should make them
-- concrete,  we could have some kind of lawless typeclass do this,
-- but here we've chosen to just provide concrete impls where we want to support it

-- once we add fixed export lists to this repo, this should NOT be exported
mkJsonWspQuery
  :: forall a. a -> QueryType -> Effect (JsonWspRequest (QueryArgs a))
mkJsonWspQuery a qt = do
  id <- _uniqueId (show qt <> "-")
  pure
    { type: "jsonwsp/request"
    , version: "1.0"
    , servicename: "ogmios"
    , methodname: "Query"
    , args: { query: a }
    , mirror: { step: "INIT", id }
    }

-- the actual query description
type UtxoQueryParams = { utxo :: Array OgmiosAddress }

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
   . DecodeAeson a
  => Aeson
  -> Either JsonDecodeError (JsonWspResponse a)
parseJsonWspResponse = aesonObject
  ( \o -> do
      typeField <- parseFieldToString o "type"
      version <- parseFieldToString o "version"
      servicename <- parseFieldToString o "servicename"
      methodname <- parseFieldToString o "methodname"
      result <- decodeAeson =<< getField o "result"
      reflection <- parseMirror =<< getField o "reflection"
      pure
        { "type": typeField
        , version
        , servicename
        , methodname
        , result
        , reflection
        }
  )

-- parses json string at a given field to an ordinary string
parseFieldToString :: Object Aeson -> String -> Either JsonDecodeError String
parseFieldToString o str =
  caseAesonString
    (Left (TypeMismatch ("expected field: '" <> str <> "' as a String")))
    Right =<<
    getField o str

-- parses a string at the given field to a UInt
parseFieldToUInt :: Object Aeson -> String -> Either JsonDecodeError UInt.UInt
parseFieldToUInt o str = do
  let err = TypeMismatch $ "expected field: '" <> str <> "' as a UInt"
  -- We use string parsing for Ogmios (AffInterface tests) but also change Medea
  -- schema and UtxoQueryResponse.json to be a string to pass (local) parsing
  -- tests. Notice "index" is a string in our local example.
  caseAesonUInt (Left err) Right =<< getField o str

-- -- The below doesn't seem to work with Ogmios query test (AffInterface)
-- -- eventhough it seems more reasonable.
-- num <- decodeNumber =<< getField o str
-- note err $ UInt.fromNumber' num
-- parses a string at the given field to a BigInt
parseFieldToBigInt
  :: Object Aeson -> String -> Either JsonDecodeError BigInt.BigInt
parseFieldToBigInt o str = do
  -- We use string parsing for Ogmios (AffInterface tests) but also change Medea
  -- schema and UtxoQueryResponse.json to be a string to pass (local) parsing
  -- tests. Notice "coins" is a string in our local example.
  let err = TypeMismatch $ "expected field: '" <> str <> "' as a BigInt"
  caseAesonBigInt (Left err) Right =<< getField o str

-- parser for the `Mirror` type.
parseMirror :: Aeson -> Either JsonDecodeError Mirror
parseMirror = caseAesonObject (Left (TypeMismatch "expected object")) $
  ( \o -> do
      step <- parseFieldToString o "step"
      id <- parseFieldToString o "id"
      pure { step, id }
  )

newtype Assets = Assets (Map CurrencySymbol (Map TokenName BigInt))

instance DecodeAeson Assets where
  decodeAeson j = do
    wspAssets :: Array (String /\ BigInt) <- FO.toUnfoldable <$> decodeAeson j
    Assets <<< Map.fromFoldableWith (Map.unionWith (+)) <$> sequence
      (uncurry decodeAsset <$> wspAssets)
    where
    decodeAsset
      :: String
      -> BigInt
      -> Either JsonDecodeError (CurrencySymbol /\ Map TokenName BigInt)
    decodeAsset assetStr quantity = do
      let
        -- Ogmios encodes CurrencySymbol and TokenName to hex strings separated with '.'
        -- TokenName part is optional
        currSymStr /\ tnStr = case indexOf (Pattern ".") assetStr of
          Nothing -> assetStr /\ ""
          Just ix ->
            let
              { before, after } = splitAt ix assetStr
              tn = fromMaybe "" $ after # uncons <#> _.tail
            in
              before /\ tn

      currSymb <- note (assetStrError assetStr "CurrencySymbol" currSymStr)
        $ mkCurrencySymbol =<< hexToByteArray currSymStr
      tokenName <- note (assetStrError assetStr "TokenName" tnStr)
        $ mkTokenName =<< hexToByteArray tnStr
      pure $ currSymb /\ Map.singleton tokenName quantity

    assetStrError str t v =
      ( TypeMismatch
          ("In " <> str <> ": Expected hex-encoded " <> t <> ", got: " <> v)
      )

-- the outer result type for Utxo queries, newtyped so that it can have
-- appropriate instances to work with `parseJsonWspResponse`
newtype UtxoQR = UtxoQR UtxoQueryResult

derive newtype instance showUtxoQR :: Show UtxoQR

instance decodeAesonUtxoQR :: DecodeAeson UtxoQR where
  decodeAeson j = UtxoQR <$> parseUtxoQueryResult j

-- the inner type for Utxo Queries
type UtxoQueryResult = Map.Map OgmiosTxOutRef OgmiosTxOut

-- Ogmios TxOutRef
type OgmiosTxOutRef =
  { txId :: String
  , index :: UInt.UInt
  }

parseUtxoQueryResult :: Aeson -> Either JsonDecodeError UtxoQueryResult
parseUtxoQueryResult = caseAesonArray (Left (TypeMismatch "Expected Array")) $
  (\array -> foldl insertFunc (Right Map.empty) array)
  where
  insertFunc
    :: Either JsonDecodeError UtxoQueryResult
    -> Aeson
    -> Either JsonDecodeError UtxoQueryResult
  insertFunc acc = caseAesonArray (Left (TypeMismatch "Expected Array")) $ inner
    where
    inner :: Array Aeson -> Either JsonDecodeError UtxoQueryResult
    inner innerArray = do
      txOutRefJson <-
        note (TypeMismatch "missing 0th element, expected an OgmiosTxOutRef") $
          index innerArray 0
      txOutJson <- note (TypeMismatch "missing 1st element, expected a TxOut") $
        index innerArray 1
      txOutRef <- parseTxOutRef txOutRefJson
      txOut <- parseTxOut txOutJson
      Map.insert txOutRef txOut <$> acc

-- helper for assuming we get an object
aesonObject
  :: forall (a :: Type)
   . (Object Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonObject = caseAesonObject (Left (TypeMismatch "expected object"))

-- parser for txOutRef
parseTxOutRef :: Aeson -> Either JsonDecodeError OgmiosTxOutRef
parseTxOutRef = aesonObject $
  ( \o -> do
      txId <- parseFieldToString o "txId"
      index <- parseFieldToUInt o "index"
      pure { txId, index }
  )

type OgmiosTxOut =
  { address :: OgmiosAddress
  , value :: Value
  , datum :: Maybe String
  }

-- Ogmios currently supplies the Raw OgmiosAddress in addr1 format, rather than the
-- cardano-serialization-lib 'OgmiosAddress' type,  perhaps this information can be
-- extracted.
parseTxOut :: Aeson -> Either JsonDecodeError OgmiosTxOut
parseTxOut = aesonObject $
  ( \o -> do
      address <- parseFieldToString o "address"
      value <- parseValue o
      let datum = hush $ parseFieldToString o "datum"
      pure $ { address, value, datum }
  )

-- parses the `Value` type
parseValue :: Object Aeson -> Either JsonDecodeError Value
parseValue outer = do
  o <- getField outer "value"
  coins <- parseFieldToBigInt o "coins" <|> Left
    (TypeMismatch "Expected 'coins' to be an Int or a BigInt")
  Assets assetsMap <- fromMaybe (Assets Map.empty) <$> getFieldOptional o
    "assets"
  pure $ mkValue (wrap coins) (wrap assetsMap)

-- chain tip query

data ChainTipQR
  = CtChainOrigin ChainOrigin
  | CtChainPoint ChainPoint

instance DecodeAeson ChainTipQR where
  decodeAeson j = do
    r :: (ChainOrigin |+| ChainPoint) <- decodeAeson j
    pure $ case toEither1 r of
      Left co -> CtChainOrigin co
      Right cp -> CtChainPoint cp

-- | A Blake2b 32-byte digest of an era-independent block header, serialised as CBOR in base16
newtype OgmiosBlockHeaderHash = OgmiosBlockHeaderHash String

derive instance Eq OgmiosBlockHeaderHash
derive newtype instance DecodeAeson OgmiosBlockHeaderHash

-- | The origin of the blockchain. It doesn't point to any existing slots, but is preceding any existing other point.
newtype ChainOrigin = ChainOrigin String

derive instance Eq ChainOrigin
derive newtype instance DecodeAeson ChainOrigin
derive newtype instance HasRuntimeType ChainOrigin

-- | A point on the chain, identified by a slot and a block header hash
type ChainPoint =
  { slot :: Slot
  , hash :: OgmiosBlockHeaderHash
  }
