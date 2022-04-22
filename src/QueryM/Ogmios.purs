-- | Provides types and instances to create Ogmios requests and decode
-- | its responses.
module QueryM.Ogmios
  ( ChainOrigin(..)
  , ChainPoint(..)
  , ChainTipQR(..)
  , OgmiosAddress
  , OgmiosBlockHeaderHash(..)
  , OgmiosTxOut(..)
  , OgmiosTxOutRef(..)
  , SubmitTxR(..)
  , TxEvaluationResult(..)
  , TxHash
  , UtxoQueryResult(..)
  , UtxoQR(..)
  , queryChainTipCall
  , queryUtxosCall
  , queryUtxosAtCall
  , submitTxCall
  , evaluateTxCall
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , caseAesonArray
  , caseAesonObject
  , decodeAeson
  , getField
  , getFieldOptional
  )
import Control.Alt ((<|>))
import Data.Argonaut (class EncodeJson, JsonDecodeError(TypeMismatch))
import Data.Array (index, singleton)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), either, hush, note)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern), indexOf, splitAt, uncons)
import Data.Traversable (sequence)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.UInt as UInt
import Foreign.Object (Object)
import Foreign.Object as FO
import QueryM.JsonWsp
  ( JsonWspCall
  , JsonWspRequest
  , mkCallType
  , parseFieldToBigInt
  , parseFieldToString
  , parseFieldToUInt
  )
import Serialization.Address (Slot)
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Types.Natural (Natural)
import Types.RedeemerTag as Tag
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

-- LOCAL STATE QUERY PROTOCOL
-- https://ogmios.dev/mini-protocols/local-state-query/

-- | Queries Ogmios for the chain’s current tip.
queryChainTipCall :: JsonWspCall Unit ChainTipQR
queryChainTipCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "chainTip" }
  }
  Proxy

-- | Queries Ogmios for utxos at given addresses.
-- NOTE. querying for utxos by address is deprecated, should use output reference instead
queryUtxosCall :: JsonWspCall { utxo :: Array OgmiosAddress } UtxoQR
queryUtxosCall = mkOgmiosCallType
  { methodname: "Query"
  , args: { query: _ }
  }
  Proxy

-- | Queries Ogmios for utxos at given address.
-- NOTE. querying for utxos by address is deprecated, should use output reference instead
queryUtxosAtCall :: JsonWspCall OgmiosAddress UtxoQR
queryUtxosAtCall = mkOgmiosCallType
  { methodname: "Query"
  , args: { query: _ } <<< { utxo: _ } <<< singleton
  }
  Proxy

type OgmiosAddress = String

-- LOCAL TX SUBMISSION
-- https://ogmios.dev/mini-protocols/local-tx-submission/

-- | Sends a serialized signed transaction with its full witness through the
-- | Cardano network via Ogmios.
submitTxCall :: JsonWspCall { txCbor :: ByteArray } SubmitTxR
submitTxCall = mkOgmiosCallType
  { methodname: "SubmitTx"
  , args: { submit: _ } <<< byteArrayToHex <<< _.txCbor
  }
  Proxy

-- | Evaluates the execution units of scripts present in a given transaction,
-- | without actually submitting the transaction.
evaluateTxCall :: JsonWspCall { txCbor :: ByteArray } TxEvaluationResult
evaluateTxCall = mkOgmiosCallType
  { methodname: "EvaluateTx"
  , args: { evaluate: _ } <<< byteArrayToHex <<< _.txCbor
  }
  Proxy

-- convenience helper
mkOgmiosCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeJson (JsonWspRequest a)
  => { methodname :: String, args :: i -> a }
  -> Proxy o
  -> JsonWspCall i o
mkOgmiosCallType = mkCallType
  { "type": "jsonwsp/request"
  , version: "1.0"
  , servicename: "ogmios"
  }

---------------- TX SUBMISSION QUERY RESPONSE & PARSING

newtype SubmitTxR = SubmitTxR TxHash

derive instance Generic SubmitTxR _
derive instance Newtype SubmitTxR _

instance Show SubmitTxR where
  show = genericShow

type TxHash = ByteArray

instance DecodeAeson SubmitTxR where
  decodeAeson = aesonObject $
    \o -> getField o "SubmitSuccess" >>= flip getField "txId" >>= hexToByteArray
      >>> maybe (Left (TypeMismatch "Expected hexstring")) (pure <<< wrap)

---------------- TX EVALUATION QUERY RESPONSE & PARSING

newtype TxEvaluationResult = TxEvaluationResult
  { "EvaluationResult" ::
      Map
        { entityRedeemerTag :: Tag.RedeemerTag, entityIndex :: Natural }
        { memory :: Natural, steps :: Natural }
  }

instance DecodeAeson TxEvaluationResult where
  decodeAeson _ = Left
    (TypeMismatch "DecodeAeson TxEvaluationResult is not implemented")

---------------- CHAIN TIP QUERY RESPONSE & PARSING

data ChainTipQR
  = CtChainOrigin ChainOrigin
  | CtChainPoint ChainPoint

derive instance Generic ChainTipQR _

instance Show ChainTipQR where
  show = genericShow

instance DecodeAeson ChainTipQR where
  decodeAeson j = do
    r :: (ChainOrigin |+| ChainPoint) <- decodeAeson j
    pure $ either CtChainOrigin CtChainPoint $ toEither1 r

-- | A Blake2b 32-byte digest of an era-independent block header, serialised as
-- CBOR in base16
newtype OgmiosBlockHeaderHash = OgmiosBlockHeaderHash String

derive instance Eq OgmiosBlockHeaderHash
derive newtype instance DecodeAeson OgmiosBlockHeaderHash
derive instance Generic OgmiosBlockHeaderHash _

instance Show OgmiosBlockHeaderHash where
  show = genericShow

-- | The origin of the blockchain. It doesn't point to any existing slots, but
-- is preceding any existing other point.
newtype ChainOrigin = ChainOrigin String

derive instance Eq ChainOrigin
derive newtype instance DecodeAeson ChainOrigin
derive newtype instance HasRuntimeType ChainOrigin
derive instance Generic ChainOrigin _

instance Show ChainOrigin where
  show = genericShow

-- | A point on the chain, identified by a slot and a block header hash
type ChainPoint =
  { slot :: Slot
  , hash :: OgmiosBlockHeaderHash
  }

---------------- UTXO QUERY RESPONSE & PARSING

-- the outer result type for Utxo queries, newtyped so that it can have
-- appropriate instances to work with `parseJsonWspResponse`
-- | Ogmios response for Utxo Query
newtype UtxoQR = UtxoQR UtxoQueryResult

derive newtype instance Show UtxoQR

instance DecodeAeson UtxoQR where
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
  foldl insertFunc (Right Map.empty)
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
parseTxOutRef = aesonObject $ \o -> do
  txId <- parseFieldToString o "txId"
  index <- parseFieldToUInt o "index"
  pure { txId, index }

type OgmiosTxOut =
  { address :: OgmiosAddress
  , value :: Value
  , datum :: Maybe String
  }

-- Ogmios currently supplies the Raw OgmiosAddress in addr1 format, rather than the
-- cardano-serialization-lib 'OgmiosAddress' type,  perhaps this information can be
-- extracted.
parseTxOut :: Aeson -> Either JsonDecodeError OgmiosTxOut
parseTxOut = aesonObject $ \o -> do
  address <- parseFieldToString o "address"
  value <- parseValue o
  let datum = hush $ parseFieldToString o "datum"
  pure $ { address, value, datum }

-- parses the `Value` type
parseValue :: Object Aeson -> Either JsonDecodeError Value
parseValue outer = do
  o <- getField outer "value"
  coins <- parseFieldToBigInt o "coins"
    <|> Left (TypeMismatch "Expected 'coins' to be an Int or a BigInt")
  Assets assetsMap <- fromMaybe (Assets Map.empty)
    <$> getFieldOptional o "assets"
  pure $ mkValue (wrap coins) (wrap assetsMap)

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
        -- Ogmios encodes CurrencySymbol and TokenName to hex strings separated
        -- with '.' TokenName part is optional
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

    assetStrError str t v = TypeMismatch $
      "In "
        <> str
        <> ": Expected hex-encoded "
        <> t
        <> ", got: "
        <> v
