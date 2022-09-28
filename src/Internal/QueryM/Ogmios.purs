-- | Provides types and instances to create Ogmios requests and decode
-- | its responses.
module Ctl.Internal.QueryM.Ogmios
  ( ChainOrigin(ChainOrigin)
  , ChainPoint
  , ChainTipQR(CtChainOrigin, CtChainPoint)
  , CostModelV1
  , CostModelV2
  , CoinsPerUtxoUnit(CoinsPerUtxoByte, CoinsPerUtxoWord)
  , CurrentEpoch(CurrentEpoch)
  , Epoch(Epoch)
  , EpochLength(EpochLength)
  , EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  , EraSummaryTime(EraSummaryTime)
  , ExecutionUnits
  , MempoolSnapshotAcquired
  , OgmiosAddress
  , OgmiosBlockHeaderHash(OgmiosBlockHeaderHash)
  , OgmiosTxOut
  , OgmiosTxOutRef
  , PParamRational(PParamRational)
  , ProtocolParameters(ProtocolParameters)
  , RedeemerPointer
  , RelativeTime(RelativeTime)
  , SafeZone(SafeZone)
  , ScriptFailure
      ( ExtraRedeemers
      , MissingRequiredDatums
      , MissingRequiredScripts
      , ValidatorFailed
      , UnknownInputReferencedByRedeemer
      , NonScriptInputReferencedByRedeemer
      , IllFormedExecutionBudget
      , NoCostModelForLanguage
      )
  , AdditionalUtxoSet(AdditionalUtxoSet)
  , OgmiosUtxoMap
  , OgmiosDatum
  , OgmiosScript
  , OgmiosTxIn
  , OgmiosTxId
  , SlotLength(SlotLength)
  , SubmitTxR(SubmitTxSuccess, SubmitFail)
  , SystemStart(SystemStart)
  , TxEvaluationFailure(UnparsedError, ScriptFailures)
  , TxEvaluationResult(TxEvaluationResult)
  , TxEvaluationR(TxEvaluationR)
  , TxHash
  , UtxoQR(UtxoQR)
  , UtxoQueryResult
  , acquireMempoolSnapshotCall
  , aesonArray
  , aesonObject
  , evaluateTxCall
  , mempoolSnapshotHasTxCall
  , mkOgmiosCallType
  , queryChainTipCall
  , queryCurrentEpochCall
  , queryEraSummariesCall
  , queryProtocolParametersCall
  , querySystemStartCall
  , queryUtxoCall
  , queryUtxosAtCall
  , queryUtxosCall
  , submitTxCall
  , slotLengthFactor
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(AtKey, TypeMismatch, MissingValue)
  , caseAesonArray
  , caseAesonObject
  , caseAesonString
  , decodeAeson
  , encodeAeson
  , encodeAeson'
  , getField
  , getFieldOptional
  , getFieldOptional'
  , isNull
  , isString
  , stringifyAeson
  , toString
  )
import Control.Alt ((<|>))
import Control.Monad.Reader.Trans (ReaderT(ReaderT), runReaderT)
import Ctl.Internal.Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  )
import Ctl.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  )
import Ctl.Internal.Cardano.Types.Transaction
  ( Costmdls(Costmdls)
  , ExUnitPrices
  , ExUnits
  , Nonce
  , SubCoin
  )
import Ctl.Internal.Cardano.Types.Transaction as T
import Ctl.Internal.Cardano.Types.Value
  ( Coin(Coin)
  , CurrencySymbol
  , Value
  , flattenNonAdaValue
  , getCurrencySymbol
  , getLovelace
  , getNonAdaAsset
  , mkCurrencySymbol
  , mkNonAdaAsset
  , mkValue
  , valueToCoin
  )
import Ctl.Internal.Helpers (encodeMap, showWithParens)
import Ctl.Internal.QueryM.JsonWsp (JsonWspCall, JsonWspRequest, mkCallType)
import Ctl.Internal.Serialization.Address (Slot)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashFromBytes)
import Ctl.Internal.Types.BigNum (fromBigInt) as BigNum
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes, cborBytesToHex)
import Ctl.Internal.Types.Int as Csl
import Ctl.Internal.Types.Natural (Natural)
import Ctl.Internal.Types.Natural (fromString) as Natural
import Ctl.Internal.Types.Rational (Rational, (%))
import Ctl.Internal.Types.Rational as Rational
import Ctl.Internal.Types.RawBytes (hexToRawBytes)
import Ctl.Internal.Types.RedeemerTag (RedeemerTag)
import Ctl.Internal.Types.RedeemerTag (fromString) as RedeemerTag
import Ctl.Internal.Types.Scripts
  ( Language(PlutusV1, PlutusV2)
  , PlutusScript(PlutusScript)
  )
import Ctl.Internal.Types.TokenName (TokenName, getTokenName, mkTokenName)
import Ctl.Internal.Types.Transaction (TransactionHash, TransactionInput)
import Data.Array (catMaybes, index, reverse, singleton)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), either, note)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as Int
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String
  ( Pattern(Pattern)
  , indexOf
  , split
  , splitAt
  , uncons
  )
import Data.String.Common (split) as String
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Foreign.Object (Object)
import Foreign.Object (toUnfoldable) as ForeignObject
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Partial.Unsafe (unsafePartial)
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (type (|+|), toEither1)

--------------------------------------------------------------------------------
-- Local State Query Protocol
-- https://ogmios.dev/mini-protocols/local-state-query/
--------------------------------------------------------------------------------

-- | Queries Ogmios for the system start Datetime
querySystemStartCall :: JsonWspCall Unit SystemStart
querySystemStartCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "systemStart" }
  }

-- | Queries Ogmios for the current epoch
queryCurrentEpochCall :: JsonWspCall Unit CurrentEpoch
queryCurrentEpochCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "currentEpoch" }
  }

-- | Queries Ogmios for an array of era summaries, used for Slot arithmetic.
queryEraSummariesCall :: JsonWspCall Unit EraSummaries
queryEraSummariesCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "eraSummaries" }
  }

-- | Queries Ogmios for the current protocol parameters
queryProtocolParametersCall :: JsonWspCall Unit ProtocolParameters
queryProtocolParametersCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "currentProtocolParameters" }
  }

-- | Queries Ogmios for the chain’s current tip.
queryChainTipCall :: JsonWspCall Unit ChainTipQR
queryChainTipCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "chainTip" }
  }

-- | Queries Ogmios for utxos at given addresses.
-- | NOTE. querying for utxos by address is deprecated, should use output reference instead
queryUtxosCall :: JsonWspCall { utxo :: Array OgmiosAddress } UtxoQR
queryUtxosCall = mkOgmiosCallType
  { methodname: "Query"
  , args: { query: _ }
  }

-- | Queries Ogmios for utxos at given address.
-- | NOTE. querying for utxos by address is deprecated, should use output reference instead
queryUtxosAtCall :: JsonWspCall OgmiosAddress UtxoQR
queryUtxosAtCall = mkOgmiosCallType
  { methodname: "Query"
  , args: { query: _ } <<< { utxo: _ } <<< singleton
  }

-- | Queries Ogmios for the utxo with the given output reference.
queryUtxoCall :: JsonWspCall TransactionInput UtxoQR
queryUtxoCall = mkOgmiosCallType
  { methodname: "Query"
  , args: { query: _ } <<< { utxo: _ } <<< singleton <<< renameFields <<< unwrap
  }
  where
  renameFields
    :: { transactionId :: TransactionHash, index :: UInt }
    -> { txId :: TransactionHash, index :: UInt }
  renameFields { transactionId: txId, index } = { txId, index }

type OgmiosAddress = String

--------------------------------------------------------------------------------
-- Local Tx Submission Protocol
-- https://ogmios.dev/mini-protocols/local-tx-submission/
--------------------------------------------------------------------------------

-- | Sends a serialized signed transaction with its full witness through the
-- | Cardano network via Ogmios.
submitTxCall :: JsonWspCall (TxHash /\ CborBytes) SubmitTxR
submitTxCall = mkOgmiosCallType
  { methodname: "SubmitTx"
  , args: { submit: _ } <<< cborBytesToHex <<< snd
  }

-- | Evaluates the execution units of scripts present in a given transaction,
-- | without actually submitting the transaction.
evaluateTxCall :: JsonWspCall (CborBytes /\ AdditionalUtxoSet) TxEvaluationR
evaluateTxCall = mkOgmiosCallType
  { methodname: "EvaluateTx"
  , args: \(cbor /\ utxoqr) ->
      { evaluate: cborBytesToHex cbor
      , additionalUtxoSet: utxoqr
      }
  }

--------------------------------------------------------------------------------
-- Local Tx Monitor Protocol
-- https://ogmios.dev/mini-protocols/local-tx-monitor/
--------------------------------------------------------------------------------

acquireMempoolSnapshotCall :: JsonWspCall Unit MempoolSnapshotAcquired
acquireMempoolSnapshotCall =
  mkOgmiosCallTypeNoArgs "AwaitAcquire"

mempoolSnapshotHasTxCall
  :: MempoolSnapshotAcquired -> JsonWspCall TxHash Boolean
mempoolSnapshotHasTxCall _ = mkOgmiosCallType
  { methodname: "HasTx"
  , args: { id: _ }
  }

--------------------------------------------------------------------------------
-- Local Tx Monitor Query Response & Parsing
--------------------------------------------------------------------------------

newtype MempoolSnapshotAcquired = AwaitAcquired Slot

instance Show MempoolSnapshotAcquired where
  show (AwaitAcquired slot) = "(AwaitAcquired " <> show slot <> ")"

instance DecodeAeson MempoolSnapshotAcquired where
  decodeAeson =
    map AwaitAcquired <<< aesonObject
      (flip getField "AwaitAcquired" >=> flip getField "slot")

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

mkOgmiosCallTypeNoArgs
  :: forall (o :: Type). String -> JsonWspCall Unit o
mkOgmiosCallTypeNoArgs methodname =
  mkOgmiosCallType { methodname, args: const {} }

mkOgmiosCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonWspRequest a)
  => { methodname :: String, args :: i -> a }
  -> JsonWspCall i o
mkOgmiosCallType =
  ( mkCallType
      { "type": "jsonwsp/request"
      , version: "1.0"
      , servicename: "ogmios"
      }
  )

---------------- TX SUBMISSION QUERY RESPONSE & PARSING

data SubmitTxR
  = SubmitTxSuccess TxHash
  | SubmitFail (Array Aeson)

derive instance Generic SubmitTxR _

instance Show SubmitTxR where
  show = genericShow

type TxHash = ByteArray

instance DecodeAeson SubmitTxR where
  decodeAeson = aesonObject $
    \o ->
      ( getField o "SubmitSuccess" >>= flip getField "txId" >>= hexToByteArray
          >>> maybe (Left (TypeMismatch "Expected hexstring"))
            (pure <<< SubmitTxSuccess)
      ) <|> (SubmitFail <$> getField o "SubmitFail")

---------------- SYSTEM START QUERY RESPONSE & PARSING
newtype SystemStart = SystemStart String

derive instance Generic SystemStart _
derive instance Newtype SystemStart _
derive newtype instance DecodeAeson SystemStart
derive newtype instance EncodeAeson SystemStart
derive newtype instance Eq SystemStart

instance Show SystemStart where
  show = genericShow

---------------- CURRENT EPOCH QUERY RESPONSE & PARSING
newtype CurrentEpoch = CurrentEpoch BigInt

derive instance Generic CurrentEpoch _
derive instance Newtype CurrentEpoch _
derive newtype instance DecodeAeson CurrentEpoch
derive newtype instance EncodeAeson CurrentEpoch
derive newtype instance Eq CurrentEpoch
derive newtype instance Ord CurrentEpoch

instance Show CurrentEpoch where
  show (CurrentEpoch ce) = showWithParens "CurrentEpoch" ce

---------------- ERA SUMMARY QUERY RESPONSE & PARSING

newtype EraSummaries = EraSummaries (Array EraSummary)

derive instance Generic EraSummaries _
derive instance Newtype EraSummaries _
derive newtype instance Eq EraSummaries
derive newtype instance EncodeAeson EraSummaries

instance Show EraSummaries where
  show = genericShow

instance DecodeAeson EraSummaries where
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
derive newtype instance Eq EraSummary

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

instance EncodeAeson EraSummary where
  encodeAeson' (EraSummary { start, end, parameters }) =
    encodeAeson'
      { "start": start
      , "end": end
      , "parameters": parameters
      }

newtype EraSummaryTime = EraSummaryTime
  { time :: RelativeTime -- The time is relative to the start time of the network.
  , slot :: Slot -- An absolute slot number
  -- Ogmios returns a number 0-18446744073709552000 but our `Slot` is a Rust
  -- u64 which has precision up to 18446744073709551615 (note 385 difference)
  -- we treat this as neglible instead of defining `AbsSlot BigInt`. See
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/632 for
  -- details.
  , epoch :: Epoch -- 0-18446744073709552000, an epoch number or length, don't
  -- use `Cardano.Types.Epoch` because Epoch is bounded by UInt also.
  }

derive instance Generic EraSummaryTime _
derive instance Newtype EraSummaryTime _
derive newtype instance Eq EraSummaryTime

instance Show EraSummaryTime where
  show = genericShow

instance DecodeAeson EraSummaryTime where
  decodeAeson = aesonObject $ \o -> do
    time <- getField o "time"
    slot <- getField o "slot"
    epoch <- getField o "epoch"
    pure $ wrap { time, slot, epoch }

instance EncodeAeson EraSummaryTime where
  encodeAeson' (EraSummaryTime { time, slot, epoch }) =
    encodeAeson'
      { "time": time
      , "slot": slot
      , "epoch": epoch
      }

-- | A time in seconds relative to another one (typically, system start or era
-- | start). [ 0 .. 18446744073709552000 ]
newtype RelativeTime = RelativeTime Number

derive instance Generic RelativeTime _
derive instance Newtype RelativeTime _
derive newtype instance Eq RelativeTime
derive newtype instance Ord RelativeTime
derive newtype instance DecodeAeson RelativeTime
derive newtype instance EncodeAeson RelativeTime

instance Show RelativeTime where
  show (RelativeTime rt) = showWithParens "RelativeTime" rt

-- | An epoch number or length with greater precision for Ogmios than
-- | `Cardano.Types.Epoch`. [ 0 .. 18446744073709552000 ]
newtype Epoch = Epoch BigInt

derive instance Generic Epoch _
derive instance Newtype Epoch _
derive newtype instance Eq Epoch
derive newtype instance Ord Epoch
derive newtype instance DecodeAeson Epoch
derive newtype instance EncodeAeson Epoch

instance Show Epoch where
  show (Epoch e) = showWithParens "Epoch" e

newtype EraSummaryParameters = EraSummaryParameters
  { epochLength :: EpochLength -- 0-18446744073709552000 An epoch number or length.
  , slotLength :: SlotLength -- <= MAX_SAFE_INTEGER (=9,007,199,254,740,992)
  -- A slot length, in milliseconds, previously it has
  -- a max limit of 18446744073709552000, now removed.
  , safeZone :: SafeZone -- 0-18446744073709552000 Number of slots from the tip of
  -- the ledger in which it is guaranteed that no hard fork can take place.
  -- This should be (at least) the number of slots in which we are guaranteed
  -- to have k blocks.
  }

derive instance Generic EraSummaryParameters _
derive instance Newtype EraSummaryParameters _
derive newtype instance Eq EraSummaryParameters

instance Show EraSummaryParameters where
  show = genericShow

instance DecodeAeson EraSummaryParameters where
  decodeAeson = aesonObject $ \o -> do
    epochLength <- getField o "epochLength"
    slotLength <- wrap <$> ((*) slotLengthFactor <$> getField o "slotLength")
    safeZone <- fromMaybe zero <$> getField o "safeZone"
    pure $ wrap { epochLength, slotLength, safeZone }

-- | The EraSummaryParameters uses seconds and we use miliseconds
-- use it to translate between them
slotLengthFactor :: Number
slotLengthFactor = 1e3

instance EncodeAeson EraSummaryParameters where
  encodeAeson' (EraSummaryParameters { epochLength, slotLength, safeZone }) =
    encodeAeson'
      { "epochLength": epochLength
      , "slotLength": slotLength
      , "safeZone": safeZone
      }

-- | An epoch number or length. [ 0 .. 18446744073709552000 ]
newtype EpochLength = EpochLength BigInt

derive instance Generic EpochLength _
derive instance Newtype EpochLength _
derive newtype instance Eq EpochLength
derive newtype instance DecodeAeson EpochLength
derive newtype instance EncodeAeson EpochLength

instance Show EpochLength where
  show (EpochLength el) = showWithParens "EpochLength" el

-- | A slot length, in milliseconds
newtype SlotLength = SlotLength Number

derive instance Generic SlotLength _
derive instance Newtype SlotLength _
derive newtype instance Eq SlotLength
derive newtype instance DecodeAeson SlotLength
derive newtype instance EncodeAeson SlotLength

instance Show SlotLength where
  show (SlotLength sl) = showWithParens "SlotLength" sl

-- | Number of slots from the tip of the ledger in which it is guaranteed that
-- | no hard fork can take place. This should be (at least) the number of slots
-- | in which we are guaranteed to have k blocks.
newtype SafeZone = SafeZone BigInt

derive instance Generic SafeZone _
derive instance Newtype SafeZone _
derive newtype instance Eq SafeZone
derive newtype instance Semiring SafeZone
derive newtype instance DecodeAeson SafeZone
derive newtype instance EncodeAeson SafeZone

instance Show SafeZone where
  show (SafeZone sz) = showWithParens "SafeZone" sz

---------------- TX EVALUATION QUERY RESPONSE & PARSING

type RedeemerPointer = { redeemerTag :: RedeemerTag, redeemerIndex :: Natural }

type ExecutionUnits = { memory :: Natural, steps :: Natural }

newtype TxEvaluationR = TxEvaluationR
  (Either TxEvaluationFailure TxEvaluationResult)

derive instance Newtype TxEvaluationR _
derive instance Generic TxEvaluationR _

instance Show TxEvaluationR where
  show = genericShow

instance DecodeAeson TxEvaluationR where
  decodeAeson aeson = (wrap <<< Right <$> decodeAeson aeson) <|>
    (wrap <<< Left <$> decodeAeson aeson)

newtype TxEvaluationResult = TxEvaluationResult
  (Map RedeemerPointer ExecutionUnits)

derive instance Newtype TxEvaluationResult _
derive instance Generic TxEvaluationResult _

instance Show TxEvaluationResult where
  show = genericShow

instance DecodeAeson TxEvaluationResult where
  decodeAeson = aesonObject $ \obj -> do
    rdmrPtrExUnitsList :: Array (String /\ Aeson) <-
      ForeignObject.toUnfoldable <$> getField obj "EvaluationResult"
    TxEvaluationResult <<< Map.fromFoldable <$>
      traverse decodeRdmrPtrExUnitsItem rdmrPtrExUnitsList
    where
    decodeRdmrPtrExUnitsItem
      :: String /\ Aeson
      -> Either JsonDecodeError (RedeemerPointer /\ ExecutionUnits)
    decodeRdmrPtrExUnitsItem (redeemerPtrRaw /\ exUnitsAeson) = do
      redeemerPtr <- decodeRedeemerPointer redeemerPtrRaw
      flip aesonObject exUnitsAeson $ \exUnitsObj -> do
        memory <- getField exUnitsObj "memory"
        steps <- getField exUnitsObj "steps"
        pure $ redeemerPtr /\ { memory, steps }

redeemerPtrTypeMismatch :: JsonDecodeError
redeemerPtrTypeMismatch = TypeMismatch
  "Expected redeemer pointer to be encoded as: \
  \^(spend|mint|certificate|withdrawal):[0-9]+$"

decodeRedeemerPointer :: String -> Either JsonDecodeError RedeemerPointer
decodeRedeemerPointer redeemerPtrRaw = note redeemerPtrTypeMismatch
  case split (Pattern ":") redeemerPtrRaw of
    [ tagRaw, indexRaw ] ->
      { redeemerTag: _, redeemerIndex: _ }
        <$> RedeemerTag.fromString tagRaw
        <*> Natural.fromString indexRaw
    _ -> Nothing

type OgmiosDatum = String
type OgmiosScript = String
type OgmiosTxId = String
type OgmiosTxIn = { txId :: OgmiosTxId, index :: Int }

data ScriptFailure
  = ExtraRedeemers (Array RedeemerPointer)
  | MissingRequiredDatums
      { provided :: Maybe (Array OgmiosDatum), missing :: Array OgmiosDatum }
  | MissingRequiredScripts
      { resolved :: Map RedeemerPointer OgmiosScript
      , missing :: Array OgmiosScript
      }
  | ValidatorFailed { error :: String, traces :: Array String }
  | UnknownInputReferencedByRedeemer OgmiosTxIn
  | NonScriptInputReferencedByRedeemer OgmiosTxIn
  | IllFormedExecutionBudget (Maybe ExecutionUnits)
  | NoCostModelForLanguage String

derive instance Generic ScriptFailure _

instance Show ScriptFailure where
  show = genericShow

-- The following cases are fine to fall through into unparsed error:
-- IncompatibleEra
-- AdditionalUtxoOverlap
-- NotEnoughSynced
-- CannotCreateEvaluationContext
data TxEvaluationFailure
  = UnparsedError String
  | ScriptFailures (Map RedeemerPointer (Array ScriptFailure))

derive instance Generic TxEvaluationFailure _

instance Show TxEvaluationFailure where
  show = genericShow

type ObjectParser = ReaderT (Object Aeson) (Either JsonDecodeError)

liftField
  :: forall (a :: Type) (b :: Type)
   . DecodeAeson a
  => String
  -> (a -> Either JsonDecodeError b)
  -> ObjectParser b
liftField f act = ReaderT (flip getField f >=> act)

instance DecodeAeson ScriptFailure where
  decodeAeson = aesonObject $ runReaderT cases
    where
    cases :: ObjectParser ScriptFailure
    cases = decodeExtraRedeemers
      <|> decodeMissingRequiredDatums
      <|> decodeMissingRequiredScripts
      <|> decodeValidatorFailed
      <|> decodeUnknownInputReferencedByRedeemer
      <|> decodeNonScriptInputReferencedByRedeemer
      <|> decodeIllFormedExecutionBudget
      <|> decodeNoCostModelForLanguage
      <|> defaultCase

    defaultCase :: ObjectParser ScriptFailure
    defaultCase = ReaderT $ const $ Left $ TypeMismatch "Expected ScriptFailure"

    decodeExtraRedeemers :: ObjectParser ScriptFailure
    decodeExtraRedeemers = ExtraRedeemers <$> liftField "extraRedeemers"
      (traverse decodeRedeemerPointer)

    decodeMissingRequiredDatums :: ObjectParser ScriptFailure
    decodeMissingRequiredDatums = liftField "missingRequiredDatums" \o -> do
      pure $ MissingRequiredDatums o

    decodeMissingRequiredScripts :: ObjectParser ScriptFailure
    decodeMissingRequiredScripts = liftField "missingRequiredScripts" \o -> do
      resolvedKV <- ForeignObject.toUnfoldable <$> getField o "resolved"
      resolved <- Map.fromFoldable <$> for (resolvedKV :: Array _)
        \(k /\ v) -> (_ /\ v) <$> decodeRedeemerPointer k
      missing <- getField o "missing"
      pure $ MissingRequiredScripts { resolved, missing }

    decodeValidatorFailed :: ObjectParser ScriptFailure
    decodeValidatorFailed = liftField "validatorFailed" \o -> do
      pure $ ValidatorFailed o

    decodeUnknownInputReferencedByRedeemer :: ObjectParser ScriptFailure
    decodeUnknownInputReferencedByRedeemer = liftField
      "unknownInputReferencedByRedeemer"
      \o -> do
        pure $ UnknownInputReferencedByRedeemer o

    decodeNonScriptInputReferencedByRedeemer :: ObjectParser ScriptFailure
    decodeNonScriptInputReferencedByRedeemer = liftField
      "nonScriptInputReferencedByRedeemer"
      \o -> do
        pure $ NonScriptInputReferencedByRedeemer o

    decodeIllFormedExecutionBudget :: ObjectParser ScriptFailure
    decodeIllFormedExecutionBudget = liftField "illFormedExecutionBudget" \o ->
      do
        pure $ IllFormedExecutionBudget o

    decodeNoCostModelForLanguage :: ObjectParser ScriptFailure
    decodeNoCostModelForLanguage = liftField "noCostModelForLanguage" \o -> do
      pure $ NoCostModelForLanguage o

instance DecodeAeson TxEvaluationFailure where
  decodeAeson = aesonObject $ runReaderT cases
    where
    cases :: ObjectParser TxEvaluationFailure
    cases = decodeScriptFailures <|> defaultCase

    defaultCase :: ObjectParser TxEvaluationFailure
    defaultCase = ReaderT \o ->
      pure (UnparsedError (stringifyAeson (encodeAeson o)))

    decodeScriptFailures :: ObjectParser TxEvaluationFailure
    decodeScriptFailures = ReaderT \o -> do
      scriptFailuresKV <- ForeignObject.toUnfoldable
        <$> (getField o "EvaluationFailure" >>= flip getField "ScriptFailures")
      scriptFailures <- Map.fromFoldable <$> for (scriptFailuresKV :: Array _)
        \(k /\ v) -> do
          v' <- decodeAeson v
          (_ /\ v') <$> decodeRedeemerPointer k
      pure $ ScriptFailures scriptFailures

---------------- PROTOCOL PARAMETERS QUERY RESPONSE & PARSING

-- | A version of `Rational` with Aeson instance that decodes from `x/y`
-- | representation, instead of `{ numerator, denominator }`
newtype PParamRational = PParamRational Rational

derive instance Newtype PParamRational _
derive instance Generic PParamRational _

instance Show PParamRational where
  show = genericShow

instance DecodeAeson PParamRational where
  decodeAeson =
    caseAesonString (Left err)
      \string -> do
        case String.split (Pattern "/") string of
          [ numeratorStr, denominatorStr ] -> note err do
            numerator <- BigInt.fromString numeratorStr
            denominator <- BigInt.fromString denominatorStr
            PParamRational <$> numerator % denominator
          _ -> Left err
    where
    err :: JsonDecodeError
    err = TypeMismatch "PParamRaional"

rationalToSubcoin :: PParamRational -> Maybe SubCoin
rationalToSubcoin (PParamRational rat) = do
  numerator <- BigNum.fromBigInt $ Rational.numerator rat
  denominator <- BigNum.fromBigInt $ Rational.denominator rat
  pure { numerator, denominator }

-- | A type that corresponds to Ogmios response.
type ProtocolParametersRaw =
  { "minFeeCoefficient" :: UInt
  , "minFeeConstant" :: UInt
  , "maxBlockBodySize" :: UInt
  , "maxBlockHeaderSize" :: UInt
  , "maxTxSize" :: UInt
  , "stakeKeyDeposit" :: BigInt
  , "poolDeposit" :: BigInt
  , "poolRetirementEpochBound" :: BigInt
  , "desiredNumberOfPools" :: BigInt
  , "poolInfluence" :: PParamRational
  , "monetaryExpansion" :: PParamRational
  , "treasuryExpansion" :: PParamRational
  , "protocolVersion" ::
      { "major" :: UInt
      , "minor" :: UInt
      }
  , "minPoolCost" :: BigInt
  , "coinsPerUtxoByte" :: Maybe BigInt
  , "coinsPerUtxoWord" :: Maybe BigInt
  , "costModels" ::
      { "plutus:v1" :: { | CostModelV1 }
      , "plutus:v2" :: Maybe { | CostModelV2 }
      }
  , "prices" ::
      { "memory" :: PParamRational
      , "steps" :: PParamRational
      }
  , "maxExecutionUnitsPerTransaction" ::
      { "memory" :: BigInt
      , "steps" :: BigInt
      }
  , "maxExecutionUnitsPerBlock" ::
      { "memory" :: BigInt
      , "steps" :: BigInt
      }
  , "maxValueSize" :: UInt
  , "collateralPercentage" :: UInt
  , "maxCollateralInputs" :: UInt
  }

data CoinsPerUtxoUnit = CoinsPerUtxoByte Coin | CoinsPerUtxoWord Coin

derive instance Generic CoinsPerUtxoUnit _

instance Show CoinsPerUtxoUnit where
  show = genericShow

-- Based on `Cardano.Api.ProtocolParameters.ProtocolParameters` from
-- `cardano-api`.
newtype ProtocolParameters = ProtocolParameters
  { protocolVersion :: UInt /\ UInt
  , decentralization :: Rational
  , extraPraosEntropy :: Maybe Nonce
  , maxBlockHeaderSize :: UInt
  , maxBlockBodySize :: UInt
  , maxTxSize :: UInt
  , txFeeFixed :: UInt
  , txFeePerByte :: UInt
  , stakeAddressDeposit :: Coin
  , stakePoolDeposit :: Coin
  , minPoolCost :: Coin
  , poolRetireMaxEpoch :: Epoch
  -- TODO: add stakePoolTargetNum :: UInt
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/571
  , poolPledgeInfluence :: Rational
  , monetaryExpansion :: Rational
  , treasuryCut :: Rational
  , coinsPerUtxoUnit :: CoinsPerUtxoUnit
  , costModels :: Costmdls
  , prices :: ExUnitPrices
  , maxTxExUnits :: ExUnits
  , maxBlockExUnits :: ExUnits
  , maxValueSize :: UInt
  , collateralPercent :: UInt
  , maxCollateralInputs :: UInt
  }

derive instance Newtype ProtocolParameters _
derive instance Generic ProtocolParameters _

instance Show ProtocolParameters where
  show = genericShow

instance DecodeAeson ProtocolParameters where
  decodeAeson aeson = do
    ps :: ProtocolParametersRaw <- decodeAeson aeson
    prices <- decodePrices ps
    coinsPerUtxoUnit <-
      maybe
        (Left $ AtKey "coinsPerUtxoByte or coinsPerUtxoWord" $ MissingValue)
        pure
        $ (CoinsPerUtxoByte <<< Coin <$> ps.coinsPerUtxoByte) <|>
            (CoinsPerUtxoWord <<< Coin <$> ps.coinsPerUtxoWord)
    pure $ ProtocolParameters
      { protocolVersion: ps.protocolVersion.major /\ ps.protocolVersion.minor
      -- The following two parameters were removed from Babbage
      , decentralization: zero
      , extraPraosEntropy: Nothing
      , maxBlockHeaderSize: ps.maxBlockHeaderSize
      , maxBlockBodySize: ps.maxBlockBodySize
      , maxTxSize: ps.maxTxSize
      , txFeeFixed: ps.minFeeConstant
      , txFeePerByte: ps.minFeeCoefficient
      , stakeAddressDeposit: Coin ps.stakeKeyDeposit
      , stakePoolDeposit: Coin ps.poolDeposit
      , minPoolCost: Coin ps.minPoolCost
      , poolRetireMaxEpoch: Epoch ps.poolRetirementEpochBound
      , poolPledgeInfluence: unwrap ps.poolInfluence
      , monetaryExpansion: unwrap ps.monetaryExpansion
      , treasuryCut: unwrap ps.treasuryExpansion -- Rational
      , coinsPerUtxoUnit: coinsPerUtxoUnit
      , costModels: Costmdls $ Map.fromFoldable $ catMaybes
          [ pure (PlutusV1 /\ convertCostModel ps.costModels."plutus:v1")
          , (PlutusV2 /\ _) <<< convertCostModel <$> ps.costModels."plutus:v2"
          ]
      , prices: prices
      , maxTxExUnits: decodeExUnits ps.maxExecutionUnitsPerTransaction
      , maxBlockExUnits: decodeExUnits ps.maxExecutionUnitsPerBlock
      , maxValueSize: ps.maxValueSize
      , collateralPercent: ps.collateralPercentage
      , maxCollateralInputs: ps.maxCollateralInputs
      }
    where
    decodeExUnits
      :: { memory :: BigInt, steps :: BigInt } -> ExUnits
    decodeExUnits { memory, steps } = { mem: memory, steps }

    decodePrices
      :: ProtocolParametersRaw -> Either JsonDecodeError ExUnitPrices
    decodePrices ps = note (TypeMismatch "ExUnitPrices") do
      memPrice <- rationalToSubcoin ps.prices.memory
      stepPrice <- rationalToSubcoin ps.prices.steps
      pure { memPrice, stepPrice } -- ExUnits

-- | A type that represents a JSON-encoded Costmodel in format used by Ogmios
type CostModelV1 =
  ( "addInteger-cpu-arguments-intercept" :: Csl.Int
  , "addInteger-cpu-arguments-slope" :: Csl.Int
  , "addInteger-memory-arguments-intercept" :: Csl.Int
  , "addInteger-memory-arguments-slope" :: Csl.Int
  , "appendByteString-cpu-arguments-intercept" :: Csl.Int
  , "appendByteString-cpu-arguments-slope" :: Csl.Int
  , "appendByteString-memory-arguments-intercept" :: Csl.Int
  , "appendByteString-memory-arguments-slope" :: Csl.Int
  , "appendString-cpu-arguments-intercept" :: Csl.Int
  , "appendString-cpu-arguments-slope" :: Csl.Int
  , "appendString-memory-arguments-intercept" :: Csl.Int
  , "appendString-memory-arguments-slope" :: Csl.Int
  , "bData-cpu-arguments" :: Csl.Int
  , "bData-memory-arguments" :: Csl.Int
  , "blake2b_256-cpu-arguments-intercept" :: Csl.Int
  , "blake2b_256-cpu-arguments-slope" :: Csl.Int
  , "blake2b_256-memory-arguments" :: Csl.Int
  , "cekApplyCost-exBudgetCPU" :: Csl.Int
  , "cekApplyCost-exBudgetMemory" :: Csl.Int
  , "cekBuiltinCost-exBudgetCPU" :: Csl.Int
  , "cekBuiltinCost-exBudgetMemory" :: Csl.Int
  , "cekConstCost-exBudgetCPU" :: Csl.Int
  , "cekConstCost-exBudgetMemory" :: Csl.Int
  , "cekDelayCost-exBudgetCPU" :: Csl.Int
  , "cekDelayCost-exBudgetMemory" :: Csl.Int
  , "cekForceCost-exBudgetCPU" :: Csl.Int
  , "cekForceCost-exBudgetMemory" :: Csl.Int
  , "cekLamCost-exBudgetCPU" :: Csl.Int
  , "cekLamCost-exBudgetMemory" :: Csl.Int
  , "cekStartupCost-exBudgetCPU" :: Csl.Int
  , "cekStartupCost-exBudgetMemory" :: Csl.Int
  , "cekVarCost-exBudgetCPU" :: Csl.Int
  , "cekVarCost-exBudgetMemory" :: Csl.Int
  , "chooseData-cpu-arguments" :: Csl.Int
  , "chooseData-memory-arguments" :: Csl.Int
  , "chooseList-cpu-arguments" :: Csl.Int
  , "chooseList-memory-arguments" :: Csl.Int
  , "chooseUnit-cpu-arguments" :: Csl.Int
  , "chooseUnit-memory-arguments" :: Csl.Int
  , "consByteString-cpu-arguments-intercept" :: Csl.Int
  , "consByteString-cpu-arguments-slope" :: Csl.Int
  , "consByteString-memory-arguments-intercept" :: Csl.Int
  , "consByteString-memory-arguments-slope" :: Csl.Int
  , "constrData-cpu-arguments" :: Csl.Int
  , "constrData-memory-arguments" :: Csl.Int
  , "decodeUtf8-cpu-arguments-intercept" :: Csl.Int
  , "decodeUtf8-cpu-arguments-slope" :: Csl.Int
  , "decodeUtf8-memory-arguments-intercept" :: Csl.Int
  , "decodeUtf8-memory-arguments-slope" :: Csl.Int
  , "divideInteger-cpu-arguments-constant" :: Csl.Int
  , "divideInteger-cpu-arguments-model-arguments-intercept" :: Csl.Int
  , "divideInteger-cpu-arguments-model-arguments-slope" :: Csl.Int
  , "divideInteger-memory-arguments-intercept" :: Csl.Int
  , "divideInteger-memory-arguments-minimum" :: Csl.Int
  , "divideInteger-memory-arguments-slope" :: Csl.Int
  , "encodeUtf8-cpu-arguments-intercept" :: Csl.Int
  , "encodeUtf8-cpu-arguments-slope" :: Csl.Int
  , "encodeUtf8-memory-arguments-intercept" :: Csl.Int
  , "encodeUtf8-memory-arguments-slope" :: Csl.Int
  , "equalsByteString-cpu-arguments-constant" :: Csl.Int
  , "equalsByteString-cpu-arguments-intercept" :: Csl.Int
  , "equalsByteString-cpu-arguments-slope" :: Csl.Int
  , "equalsByteString-memory-arguments" :: Csl.Int
  , "equalsData-cpu-arguments-intercept" :: Csl.Int
  , "equalsData-cpu-arguments-slope" :: Csl.Int
  , "equalsData-memory-arguments" :: Csl.Int
  , "equalsInteger-cpu-arguments-intercept" :: Csl.Int
  , "equalsInteger-cpu-arguments-slope" :: Csl.Int
  , "equalsInteger-memory-arguments" :: Csl.Int
  , "equalsString-cpu-arguments-constant" :: Csl.Int
  , "equalsString-cpu-arguments-intercept" :: Csl.Int
  , "equalsString-cpu-arguments-slope" :: Csl.Int
  , "equalsString-memory-arguments" :: Csl.Int
  , "fstPair-cpu-arguments" :: Csl.Int
  , "fstPair-memory-arguments" :: Csl.Int
  , "headList-cpu-arguments" :: Csl.Int
  , "headList-memory-arguments" :: Csl.Int
  , "iData-cpu-arguments" :: Csl.Int
  , "iData-memory-arguments" :: Csl.Int
  , "ifThenElse-cpu-arguments" :: Csl.Int
  , "ifThenElse-memory-arguments" :: Csl.Int
  , "indexByteString-cpu-arguments" :: Csl.Int
  , "indexByteString-memory-arguments" :: Csl.Int
  , "lengthOfByteString-cpu-arguments" :: Csl.Int
  , "lengthOfByteString-memory-arguments" :: Csl.Int
  , "lessThanByteString-cpu-arguments-intercept" :: Csl.Int
  , "lessThanByteString-cpu-arguments-slope" :: Csl.Int
  , "lessThanByteString-memory-arguments" :: Csl.Int
  , "lessThanEqualsByteString-cpu-arguments-intercept" :: Csl.Int
  , "lessThanEqualsByteString-cpu-arguments-slope" :: Csl.Int
  , "lessThanEqualsByteString-memory-arguments" :: Csl.Int
  , "lessThanEqualsInteger-cpu-arguments-intercept" :: Csl.Int
  , "lessThanEqualsInteger-cpu-arguments-slope" :: Csl.Int
  , "lessThanEqualsInteger-memory-arguments" :: Csl.Int
  , "lessThanInteger-cpu-arguments-intercept" :: Csl.Int
  , "lessThanInteger-cpu-arguments-slope" :: Csl.Int
  , "lessThanInteger-memory-arguments" :: Csl.Int
  , "listData-cpu-arguments" :: Csl.Int
  , "listData-memory-arguments" :: Csl.Int
  , "mapData-cpu-arguments" :: Csl.Int
  , "mapData-memory-arguments" :: Csl.Int
  , "mkCons-cpu-arguments" :: Csl.Int
  , "mkCons-memory-arguments" :: Csl.Int
  , "mkNilData-cpu-arguments" :: Csl.Int
  , "mkNilData-memory-arguments" :: Csl.Int
  , "mkNilPairData-cpu-arguments" :: Csl.Int
  , "mkNilPairData-memory-arguments" :: Csl.Int
  , "mkPairData-cpu-arguments" :: Csl.Int
  , "mkPairData-memory-arguments" :: Csl.Int
  , "modInteger-cpu-arguments-constant" :: Csl.Int
  , "modInteger-cpu-arguments-model-arguments-intercept" :: Csl.Int
  , "modInteger-cpu-arguments-model-arguments-slope" :: Csl.Int
  , "modInteger-memory-arguments-intercept" :: Csl.Int
  , "modInteger-memory-arguments-minimum" :: Csl.Int
  , "modInteger-memory-arguments-slope" :: Csl.Int
  , "multiplyInteger-cpu-arguments-intercept" :: Csl.Int
  , "multiplyInteger-cpu-arguments-slope" :: Csl.Int
  , "multiplyInteger-memory-arguments-intercept" :: Csl.Int
  , "multiplyInteger-memory-arguments-slope" :: Csl.Int
  , "nullList-cpu-arguments" :: Csl.Int
  , "nullList-memory-arguments" :: Csl.Int
  , "quotientInteger-cpu-arguments-constant" :: Csl.Int
  , "quotientInteger-cpu-arguments-model-arguments-intercept" :: Csl.Int
  , "quotientInteger-cpu-arguments-model-arguments-slope" :: Csl.Int
  , "quotientInteger-memory-arguments-intercept" :: Csl.Int
  , "quotientInteger-memory-arguments-minimum" :: Csl.Int
  , "quotientInteger-memory-arguments-slope" :: Csl.Int
  , "remainderInteger-cpu-arguments-constant" :: Csl.Int
  , "remainderInteger-cpu-arguments-model-arguments-intercept" :: Csl.Int
  , "remainderInteger-cpu-arguments-model-arguments-slope" :: Csl.Int
  , "remainderInteger-memory-arguments-intercept" :: Csl.Int
  , "remainderInteger-memory-arguments-minimum" :: Csl.Int
  , "remainderInteger-memory-arguments-slope" :: Csl.Int
  , "sha2_256-cpu-arguments-intercept" :: Csl.Int
  , "sha2_256-cpu-arguments-slope" :: Csl.Int
  , "sha2_256-memory-arguments" :: Csl.Int
  , "sha3_256-cpu-arguments-intercept" :: Csl.Int
  , "sha3_256-cpu-arguments-slope" :: Csl.Int
  , "sha3_256-memory-arguments" :: Csl.Int
  , "sliceByteString-cpu-arguments-intercept" :: Csl.Int
  , "sliceByteString-cpu-arguments-slope" :: Csl.Int
  , "sliceByteString-memory-arguments-intercept" :: Csl.Int
  , "sliceByteString-memory-arguments-slope" :: Csl.Int
  , "sndPair-cpu-arguments" :: Csl.Int
  , "sndPair-memory-arguments" :: Csl.Int
  , "subtractInteger-cpu-arguments-intercept" :: Csl.Int
  , "subtractInteger-cpu-arguments-slope" :: Csl.Int
  , "subtractInteger-memory-arguments-intercept" :: Csl.Int
  , "subtractInteger-memory-arguments-slope" :: Csl.Int
  , "tailList-cpu-arguments" :: Csl.Int
  , "tailList-memory-arguments" :: Csl.Int
  , "trace-cpu-arguments" :: Csl.Int
  , "trace-memory-arguments" :: Csl.Int
  , "unBData-cpu-arguments" :: Csl.Int
  , "unBData-memory-arguments" :: Csl.Int
  , "unConstrData-cpu-arguments" :: Csl.Int
  , "unConstrData-memory-arguments" :: Csl.Int
  , "unIData-cpu-arguments" :: Csl.Int
  , "unIData-memory-arguments" :: Csl.Int
  , "unListData-cpu-arguments" :: Csl.Int
  , "unListData-memory-arguments" :: Csl.Int
  , "unMapData-cpu-arguments" :: Csl.Int
  , "unMapData-memory-arguments" :: Csl.Int
  , "verifyEd25519Signature-cpu-arguments-intercept" :: Csl.Int
  , "verifyEd25519Signature-cpu-arguments-slope" :: Csl.Int
  , "verifyEd25519Signature-memory-arguments" :: Csl.Int
  )

type CostModelV2 =
  ( "serialiseData-cpu-arguments-intercept" :: Csl.Int
  , "serialiseData-cpu-arguments-slope" :: Csl.Int
  , "serialiseData-memory-arguments-intercept" :: Csl.Int
  , "serialiseData-memory-arguments-slope" :: Csl.Int
  , "verifyEcdsaSecp256k1Signature-cpu-arguments" :: Csl.Int
  , "verifyEcdsaSecp256k1Signature-memory-arguments" :: Csl.Int
  , "verifySchnorrSecp256k1Signature-cpu-arguments-intercept" :: Csl.Int
  , "verifySchnorrSecp256k1Signature-cpu-arguments-slope" :: Csl.Int
  , "verifySchnorrSecp256k1Signature-memory-arguments" :: Csl.Int
  | CostModelV1
  )

-- This assumes that cost models are stored in lexicographical order
convertCostModel
  :: forall costModel
   . HFoldl (List Csl.Int -> Csl.Int -> List Csl.Int) (List Csl.Int) costModel
       (List Csl.Int)
  => costModel
  -> T.CostModel
convertCostModel model = wrap $ reverse $ List.toUnfoldable $ hfoldl
  ((\xs x -> x List.: xs) :: List Csl.Int -> Csl.Int -> List Csl.Int)
  (mempty :: List Csl.Int)
  model

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
  { slot :: Slot -- See https://github.com/Plutonomicon/cardano-transaction-lib/issues/632
  -- for details on why we lose a neglible amount of precision.
  , hash :: OgmiosBlockHeaderHash
  }

---------------- ADDITIONAL UTXO MAP REQUEST

newtype AdditionalUtxoSet = AdditionalUtxoSet OgmiosUtxoMap

derive newtype instance Show AdditionalUtxoSet

type OgmiosUtxoMap = Map OgmiosTxOutRef OgmiosTxOut

instance EncodeAeson AdditionalUtxoSet where
  encodeAeson' (AdditionalUtxoSet m) =
    encodeAeson' $ encode <$> utxos

    where
    utxos :: Array (OgmiosTxOutRef /\ OgmiosTxOut)
    utxos = Map.toUnfoldable m

    encode (inp /\ out) =
      { "txId": inp.txId
      , "index": inp.index
      }
        /\
          { "address": out.address
          , "datumHash": out.datumHash
          , "value":
              { "coins": out.value # valueToCoin # getLovelace
              , "assets": out.value # getNonAdaAsset # encodeNonAdaAsset
              }
          }

    encodeNonAdaAsset assets = encodeMap $
      foldl
        (\m' (cs /\ tn /\ n) -> Map.insert (createKey cs tn) n m')
        Map.empty
        (flattenNonAdaValue assets)
      where
      createKey cs tn =
        if tn' == mempty then csHex else csHex <> "." <> tnHex
        where
        tn' = getTokenName tn
        cs' = getCurrencySymbol cs
        csHex = byteArrayToHex cs'
        tnHex = byteArrayToHex tn'

---------------- UTXO QUERY RESPONSE & PARSING

-- the outer result type for Utxo queries, newtyped so that it can have
-- appropriate instances to work with `parseJsonWspResponse`
-- | Ogmios response for Utxo Query
newtype UtxoQR = UtxoQR UtxoQueryResult

derive instance Newtype UtxoQR _
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
  txId <- getField o "txId"
  index <- getField o "index"
  pure { txId, index }

type OgmiosTxOut =
  { address :: OgmiosAddress
  , value :: Value
  , datumHash :: Maybe String
  , datum :: Maybe String
  , script :: Maybe ScriptRef
  }

-- Ogmios currently supplies the Raw OgmiosAddress in addr1 format, rather than the
-- cardano-serialization-lib 'OgmiosAddress' type,  perhaps this information can be
-- extracted.
parseTxOut :: Aeson -> Either JsonDecodeError OgmiosTxOut
parseTxOut = aesonObject $ \o -> do
  address <- getField o "address"
  value <- parseValue o
  datumHash <- getFieldOptional' o "datumHash"
  datum <- getFieldOptional' o "datum"
  script <- parseScript o
  pure { address, value, datumHash, datum, script }

parseScript :: Object Aeson -> Either JsonDecodeError (Maybe ScriptRef)
parseScript outer =
  getFieldOptional' outer "script" >>= case _ of
    Nothing -> pure Nothing
    Just script -> do
      case (Array.head $ ForeignObject.toUnfoldable script) of
        Just ("plutus:v1" /\ plutusScript) ->
          Just <$> parsePlutusScriptWithLang PlutusV1 plutusScript

        Just ("plutus:v2" /\ plutusScript) ->
          Just <$> parsePlutusScriptWithLang PlutusV2 plutusScript

        Just ("native" /\ nativeScript) ->
          Just <<< NativeScriptRef <$> parseNativeScript nativeScript

        _ ->
          Left $ TypeMismatch $
            "Expected native or Plutus script, got: " <> show script
  where
  parsePlutusScriptWithLang
    :: Language -> Aeson -> Either JsonDecodeError ScriptRef
  parsePlutusScriptWithLang lang aeson = do
    let
      scriptTypeMismatch :: JsonDecodeError
      scriptTypeMismatch = TypeMismatch
        $ "Expected hex-encoded Plutus script, got: " <> show aeson

    aeson # caseAesonString (Left scriptTypeMismatch)
      \hexEncodedScript -> do
        scriptBytes <- note scriptTypeMismatch (hexToByteArray hexEncodedScript)
        pure $ PlutusScriptRef $ PlutusScript (scriptBytes /\ lang)

  parseNativeScript :: Aeson -> Either JsonDecodeError NativeScript
  parseNativeScript aeson
    | isString aeson = do
        let
          pubKeyHashTypeMismatch :: JsonDecodeError
          pubKeyHashTypeMismatch = TypeMismatch
            $ "Expected hex-encoded pub key hash, got: " <> show aeson

          pubKeyHashHex :: String
          pubKeyHashHex = unsafePartial fromJust $ toString aeson

        ScriptPubkey <$> note pubKeyHashTypeMismatch
          (ed25519KeyHashFromBytes =<< hexToRawBytes pubKeyHashHex)

    | otherwise = aeson # aesonObject \obj -> do
        let
          scriptTypeMismatch :: JsonDecodeError
          scriptTypeMismatch = TypeMismatch
            $ "Expected native script, got: " <> show aeson

        case (Array.head $ ForeignObject.toUnfoldable obj) of
          Just ("any" /\ scripts) ->
            scripts # aesonArray (map ScriptAny <<< traverse parseNativeScript)

          Just ("all" /\ scripts) ->
            scripts # aesonArray (map ScriptAll <<< traverse parseNativeScript)

          Just ("expiresAt" /\ slot) ->
            TimelockExpiry <$> decodeAeson slot

          Just ("startsAt" /\ slot) ->
            TimelockStart <$> decodeAeson slot

          Just (atLeast /\ scripts) -> do
            n <- note scriptTypeMismatch (Int.fromString atLeast)
            scripts # aesonArray
              (map (ScriptNOfK n) <<< traverse parseNativeScript)

          _ -> Left scriptTypeMismatch

-- parses the `Value` type
parseValue :: Object Aeson -> Either JsonDecodeError Value
parseValue outer = do
  o <- getField outer "value"
  coins <- getField o "coins"
    <|> Left (TypeMismatch "Expected 'coins' to be an Int or a BigInt")
  Assets assetsMap <- fromMaybe (Assets Map.empty)
    <$> getFieldOptional o "assets"
  pure $ mkValue (wrap coins) (mkNonAdaAsset assetsMap)

newtype Assets = Assets (Map CurrencySymbol (Map TokenName BigInt))

instance DecodeAeson Assets where
  decodeAeson j = do
    wspAssets :: Array (String /\ BigInt) <-
      ForeignObject.toUnfoldable <$> decodeAeson j
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
