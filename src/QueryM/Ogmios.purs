-- | Provides types and instances to create Ogmios requests and decode
-- | its responses.
module QueryM.Ogmios
  ( AbsSlot(..)
  , ChainOrigin(..)
  , ChainPoint(..)
  , ChainTipQR(..)
  , CurrentEpochQR(..)
  , Epoch(..)
  , EpochLength(..)
  , EraSummariesQR(..)
  , EraSummary(..)
  , EraSummaryParameters(..)
  , EraSummaryTime(..)
  , OgmiosAddress
  , OgmiosBlockHeaderHash(..)
  , OgmiosTxOut(..)
  , OgmiosTxOutRef(..)
  , RelativeTime(..)
  , SafeZone(..)
  , SlotLength(..)
  , SubmitTxR(..)
  , SystemStartQR(..)
  , TxEvaluationR(..)
  , TxHash
  , UtxoQR(..)
  , UtxoQueryResult(..)
  , evaluateTxCall
  , queryChainTipCall
  , queryCurrentEpochCall
  , queryEraSummariesCall
  , querySystemStartCall
  , queryUtxosAtCall
  , queryUtxosCall
  , submitTxCall
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
  , getField
  , getFieldOptional
  , isNull
  )
import Control.Alt ((<|>))
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
import Data.Traversable (sequence, traverse)
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
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray (ByteArray, hexToByteArray)
import Types.CborBytes (CborBytes, cborBytesToHex)
import Types.Natural (Natural)
import Types.RedeemerTag as Tag
import Cardano.Types.Value (CurrencySymbol, Value, mkCurrencySymbol, mkValue)
import Types.TokenName (TokenName, mkTokenName)
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (type (|+|), toEither1)

-- LOCAL STATE QUERY PROTOCOL
-- https://ogmios.dev/mini-protocols/local-state-query/

-- | Queries Ogmios for the system start Datetime
querySystemStartCall :: JsonWspCall Unit SystemStartQR
querySystemStartCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "systemStart" }
  }
  Proxy

-- | Queries Ogmios for the current epoch
queryCurrentEpochCall :: JsonWspCall Unit CurrentEpochQR
queryCurrentEpochCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "currentEpoch" }
  }
  Proxy

-- | Queries Ogmios for an array of era summaries, used for Slot arithmetic.
queryEraSummariesCall :: JsonWspCall Unit EraSummariesQR
queryEraSummariesCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "eraSummaries" }
  }
  Proxy

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
submitTxCall :: JsonWspCall CborBytes SubmitTxR
submitTxCall = mkOgmiosCallType
  { methodname: "SubmitTx"
  , args: { submit: _ } <<< cborBytesToHex
  }
  Proxy

-- | Evaluates the execution units of scripts present in a given transaction,
-- | without actually submitting the transaction.
evaluateTxCall :: JsonWspCall CborBytes TxEvaluationR
evaluateTxCall = mkOgmiosCallType
  { methodname: "EvaluateTx"
  , args: { evaluate: _ } <<< cborBytesToHex
  }
  Proxy

-- convenience helper
mkOgmiosCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonWspRequest a)
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

---------------- SYSTEM START QUERY RESPONSE & PARSING
newtype SystemStartQR = SystemStartQR String

derive instance Generic SystemStartQR _
derive instance Newtype SystemStartQR _
derive newtype instance DecodeAeson SystemStartQR
derive newtype instance Eq SystemStartQR

instance Show SystemStartQR where
  show = genericShow

---------------- CURRENT EPOCH QUERY RESPONSE & PARSING
newtype CurrentEpochQR = CurrentEpochQR BigInt

derive instance Generic CurrentEpochQR _
derive instance Newtype CurrentEpochQR _
derive newtype instance DecodeAeson CurrentEpochQR
derive newtype instance Eq CurrentEpochQR
derive newtype instance Ord CurrentEpochQR

instance Show CurrentEpochQR where
  show = genericShow

---------------- ERA SUMMARY QUERY RESPONSE & PARSING

newtype EraSummariesQR = EraSummariesQR (Array EraSummary)

derive instance Generic EraSummariesQR _
derive instance Newtype EraSummariesQR _

instance Show EraSummariesQR where
  show = genericShow

instance DecodeAeson EraSummariesQR where
  decodeAeson = aesonArray (map wrap <<< traverse decodeAeson)

-- | From Ogmios:
-- | start: An era bound which captures the time, slot and epoch at which the
-- | era start. The time is relative to the start time of the network.
-- |
-- | end: An era bound which captures the time, slot and epoch at which the
-- | era start. The time is relative to the start time of the network.
-- |
-- | parameters: Parameters that can vary across hard forks.
newtype EraSummary = EraSummary
  { start :: EraSummaryTime
  , end :: Maybe EraSummaryTime
  , parameters :: EraSummaryParameters
  }

derive instance Generic EraSummary _
derive instance Newtype EraSummary _

instance Show EraSummary where
  show = genericShow

instance DecodeAeson EraSummary where
  decodeAeson = aesonObject $ \o -> do
    start <- getField o "start"
    -- The field "end" is required by Ogmios API, but it can optionally return
    -- Null, so we want to fail if the field is absent but make Null value
    -- acceptable in presence of the field (hence why "end" is wrapped in
    -- `Maybe`).
    end' <- getField o "end"
    end <- if isNull end' then pure Nothing else Just <$> decodeAeson end'
    parameters <- getField o "parameters"
    pure $ wrap { start, end, parameters }

newtype EraSummaryTime = EraSummaryTime
  { time :: RelativeTime -- 0-18446744073709552000, A time in seconds relative to
  -- another one (typically, system start or era start).
  , slot :: AbsSlot -- 0-18446744073709552000, An absolute slot number. don't
  -- use `Slot` because Slot is bounded by UInt ~ 0-4294967295.
  , epoch :: Epoch -- 0-18446744073709552000, an epoch number or length, don't
  -- use `Cardano.Types.Epoch` because Epoch is bounded by UInt also.
  }

derive instance Generic EraSummaryTime _
derive instance Newtype EraSummaryTime _

instance Show EraSummaryTime where
  show = genericShow

instance DecodeAeson EraSummaryTime where
  decodeAeson = aesonObject $ \o -> do
    time <- getField o "time"
    slot <- getField o "slot"
    epoch <- getField o "epoch"
    pure $ wrap { time, slot, epoch }

-- | A time in seconds relative to another one (typically, system start or era
-- | start). [ 0 .. 18446744073709552000 ]
newtype RelativeTime = RelativeTime BigInt

derive instance Generic RelativeTime _
derive instance Newtype RelativeTime _
derive newtype instance Eq RelativeTime
derive newtype instance Ord RelativeTime
derive newtype instance DecodeAeson RelativeTime

instance Show RelativeTime where
  show = genericShow

-- | Absolute slot relative to SystemStartQR. [ 0 .. 18446744073709552000 ]
newtype AbsSlot = AbsSlot BigInt

derive instance Generic AbsSlot _
derive instance Newtype AbsSlot _
derive newtype instance Eq AbsSlot
derive newtype instance Ord AbsSlot
derive newtype instance DecodeAeson AbsSlot

instance Show AbsSlot where
  show = genericShow

-- | An epoch number or length with greater precision for Ogmios than
-- | `Cardano.Types.Epoch`. [ 0 .. 18446744073709552000 ]
newtype Epoch = Epoch BigInt

derive instance Generic Epoch _
derive instance Newtype Epoch _
derive newtype instance Eq Epoch
derive newtype instance Ord Epoch
derive newtype instance DecodeAeson Epoch

instance Show Epoch where
  show = genericShow

newtype EraSummaryParameters = EraSummaryParameters
  { epochLength :: EpochLength -- 0-18446744073709552000 An epoch number or length.
  , slotLength :: SlotLength -- <= 18446744073709552000 A slot length, in seconds.
  , safeZone :: SafeZone -- 0-18446744073709552000 Number of slots from the tip of
  -- the ledger in which it is guaranteed that no hard fork can take place.
  -- This should be (at least) the number of slots in which we are guaranteed
  -- to have k blocks.
  }

derive instance Generic EraSummaryParameters _
derive instance Newtype EraSummaryParameters _

instance Show EraSummaryParameters where
  show = genericShow

instance DecodeAeson EraSummaryParameters where
  decodeAeson = aesonObject $ \o -> do
    epochLength <- getField o "epochLength"
    slotLength <- getField o "slotLength"
    safeZone <- getField o "safeZone"
    pure $ wrap { epochLength, slotLength, safeZone }

-- | An epoch number or length. [ 0 .. 18446744073709552000 ]
newtype EpochLength = EpochLength BigInt

derive instance Generic EpochLength _
derive instance Newtype EpochLength _
derive newtype instance DecodeAeson EpochLength

instance Show EpochLength where
  show = genericShow

-- | A slot length, in seconds <= 18446744073709552000
newtype SlotLength = SlotLength BigInt

derive instance Generic SlotLength _
derive instance Newtype SlotLength _
derive newtype instance DecodeAeson SlotLength

instance Show SlotLength where
  show = genericShow

-- | Number of slots from the tip of the ledger in which it is guaranteed that
-- | no hard fork can take place. This should be (at least) the number of slots
-- | in which we are guaranteed to have k blocks.
newtype SafeZone = SafeZone BigInt

derive instance Generic SafeZone _
derive instance Newtype SafeZone _
derive newtype instance DecodeAeson SafeZone

instance Show SafeZone where
  show = genericShow

---------------- TX EVALUATION QUERY RESPONSE & PARSING

newtype TxEvaluationR = TxEvaluationR
  { "EvaluationResult" ::
      Map
        { entityRedeemerTag :: Tag.RedeemerTag, entityIndex :: Natural }
        { memory :: Natural, steps :: Natural }
  }

instance DecodeAeson TxEvaluationR where
  decodeAeson _ = Left
    (TypeMismatch "DecodeAeson TxEvaluationR is not implemented")

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
derive instance Newtype OgmiosBlockHeaderHash _

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
  { slot :: AbsSlot -- I think we need to use `AbsSlot` here, 18446744073709552000
  -- is outside of `Slot` range.
  , hash :: OgmiosBlockHeaderHash
  }

---------------- UTXO QUERY RESPONSE & PARSING

-- the outer result type for Utxo queries, newtyped so that it can have
-- appropriate instances to work with `parseJsonWspResponse`
-- | Ogmios response for Utxo Query
newtype UtxoQR = UtxoQR UtxoQueryResult

derive newtype instance Show UtxoQR

instance DecodeAeson UtxoQR where
  decodeAeson = map UtxoQR <<< parseUtxoQueryResult

-- the inner type for Utxo Queries
type UtxoQueryResult = Map.Map OgmiosTxOutRef OgmiosTxOut

-- Ogmios tx input
type OgmiosTxOutRef =
  { txId :: String
  , index :: UInt.UInt
  }

parseUtxoQueryResult :: Aeson -> Either JsonDecodeError UtxoQueryResult
parseUtxoQueryResult = aesonArray $ foldl insertFunc (Right Map.empty)
  where
  insertFunc
    :: Either JsonDecodeError UtxoQueryResult
    -> Aeson
    -> Either JsonDecodeError UtxoQueryResult
  insertFunc acc = aesonArray inner
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
aesonObject = caseAesonObject (Left (TypeMismatch "Expected Object"))

-- helper for assumming we get an array
aesonArray
  :: forall (a :: Type)
   . (Array Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonArray = caseAesonArray (Left (TypeMismatch "Expected Array"))

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
