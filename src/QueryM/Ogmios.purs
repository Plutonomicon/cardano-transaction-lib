-- | Provides types and instances to create Ogmios requests and decode
-- | its responses.
module QueryM.Ogmios
  ( AbsSlot(..)
  , ChainOrigin(..)
  , ChainPoint(..)
  , ChainTipQR(..)
  , CostModel
  , CurrentEpoch(..)
  , Epoch(..)
  , EpochLength(..)
  , EraSummaries(..)
  , EraSummary(..)
  , EraSummaryParameters(..)
  , EraSummaryTime(..)
  , OgmiosAddress
  , OgmiosBlockHeaderHash(..)
  , OgmiosTxOut(..)
  , OgmiosTxOutRef(..)
  , PParamRational(..)
  , ProtocolParameters(..)
  , RelativeTime(..)
  , SafeZone(..)
  , SlotLength(..)
  , SubmitTxR(..)
  , SystemStart(..)
  , TxEvaluationR(..)
  , TxHash
  , UtxoQR(..)
  , UtxoQueryResult(..)
  , aesonArray
  , aesonObject
  , evaluateTxCall
  , queryProtocolParametersCall
  , queryChainTipCall
  , queryCurrentEpochCall
  , queryEraSummariesCall
  , querySystemStartCall
  , queryUtxosAtCall
  , queryUtxosCall
  , submitTxCall
  , mkOgmiosCallType
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonArray
  , caseAesonObject
  , caseAesonString
  , decodeAeson
  , encodeAeson'
  , getField
  , getFieldOptional
  , isNull
  )
import Cardano.Types.Transaction
  ( Costmdls(Costmdls)
  , ExUnitPrices
  , ExUnits
  , Language(PlutusV1)
  , Nonce
  , SubCoin
  )
import Cardano.Types.Transaction as T
import Cardano.Types.Value
  ( Coin(Coin)
  , CurrencySymbol
  , Value
  , mkCurrencySymbol
  , mkValue
  )
import Control.Alt ((<|>))
import Data.Array (index, singleton)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), either, hush, note)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern), indexOf, splitAt, uncons)
import Data.String.Common as String
import Data.Traversable (sequence, traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Foreign.Object (Object)
import Foreign.Object as FO
import Helpers (showWithParens)
import QueryM.JsonWsp
  ( JsonWspCall
  , JsonWspRequest
  , mkCallType
  )
import Type.Proxy (Proxy(Proxy))
import Types.BigNum (fromBigInt) as BigNum
import Types.ByteArray (ByteArray, hexToByteArray)
import Types.CborBytes (CborBytes, cborBytesToHex)
import Types.Natural (Natural)
import Types.Rational (Rational, (%))
import Types.Rational as Rational
import Types.RedeemerTag as Tag
import Types.TokenName (TokenName, mkTokenName)
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (type (|+|), toEither1)

-- LOCAL STATE QUERY PROTOCOL
-- https://ogmios.dev/mini-protocols/local-state-query/

-- | Queries Ogmios for the system start Datetime
querySystemStartCall :: JsonWspCall Unit SystemStart
querySystemStartCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "systemStart" }
  }
  Proxy

-- | Queries Ogmios for the current epoch
queryCurrentEpochCall :: JsonWspCall Unit CurrentEpoch
queryCurrentEpochCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "currentEpoch" }
  }
  Proxy

-- | Queries Ogmios for an array of era summaries, used for Slot arithmetic.
queryEraSummariesCall :: JsonWspCall Unit EraSummaries
queryEraSummariesCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "eraSummaries" }
  }
  Proxy

-- | Queries Ogmios for the current protocol parameters
queryProtocolParametersCall :: JsonWspCall Unit ProtocolParameters
queryProtocolParametersCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "currentProtocolParameters" }
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
  { time :: RelativeTime -- 0-18446744073709552000, The time is relative to the
  -- start time of the network.
  , slot :: AbsSlot -- 0-18446744073709552000, An absolute slot number. don't
  -- use `Slot` because Slot is bounded by UInt ~ 0-4294967295.
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
newtype RelativeTime = RelativeTime BigInt

derive instance Generic RelativeTime _
derive instance Newtype RelativeTime _
derive newtype instance Eq RelativeTime
derive newtype instance Ord RelativeTime
derive newtype instance DecodeAeson RelativeTime
derive newtype instance EncodeAeson RelativeTime

instance Show RelativeTime where
  show (RelativeTime rt) = showWithParens "RelativeTime" rt

-- | Absolute slot relative to SystemStart. [ 0 .. 18446744073709552000 ]
newtype AbsSlot = AbsSlot BigInt

derive instance Generic AbsSlot _
derive instance Newtype AbsSlot _
derive newtype instance Eq AbsSlot
derive newtype instance Ord AbsSlot
derive newtype instance DecodeAeson AbsSlot
derive newtype instance EncodeAeson AbsSlot

instance Show AbsSlot where
  show (AbsSlot as) = showWithParens "AbsSlot" as

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
  , slotLength :: SlotLength -- <= 18446744073709552000 A slot length, in seconds.
  , safeZone ::
      Maybe SafeZone -- 0-18446744073709552000 Number of slots from the tip of
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
    slotLength <- getField o "slotLength"
    safeZone <- getField o "safeZone"
    pure $ wrap { epochLength, slotLength, safeZone }

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

-- | A slot length, in seconds <= 18446744073709552000
newtype SlotLength = SlotLength BigInt

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
derive newtype instance DecodeAeson SafeZone
derive newtype instance EncodeAeson SafeZone

instance Show SafeZone where
  show (SafeZone sz) = showWithParens "SafeZone" sz

---------------- TX EVALUATION QUERY RESPONSE & PARSING

newtype TxEvaluationR = TxEvaluationR
  { "EvaluationResult" ::
      Map
        { entityRedeemerTag :: Tag.RedeemerTag, entityIndex :: Natural }
        { memory :: Natural, steps :: Natural }
  }

derive instance Generic TxEvaluationR _

instance Show TxEvaluationR where
  show = genericShow

instance DecodeAeson TxEvaluationR where
  decodeAeson _ = Left
    (TypeMismatch "DecodeAeson TxEvaluationR is not implemented")

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
  , "coinsPerUtxoByte" :: BigInt
  , "costModels" ::
      { "plutus:v1" :: CostModel
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
  , "maxValueSize" :: Maybe UInt
  , "collateralPercentage" :: Maybe UInt
  , "maxCollateralInputs" :: Maybe UInt
  }

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
  , uTxOCostPerWord :: Coin
  , costModels :: Costmdls
  , prices :: Maybe ExUnitPrices
  , maxTxExUnits :: Maybe ExUnits
  , maxBlockExUnits :: Maybe ExUnits
  , maxValueSize :: Maybe UInt
  , collateralPercent :: Maybe UInt
  , maxCollateralInputs :: Maybe UInt
  }

derive instance Newtype ProtocolParameters _
derive instance Generic ProtocolParameters _

instance Show ProtocolParameters where
  show = genericShow

instance DecodeAeson ProtocolParameters where
  decodeAeson aeson = do
    ps :: ProtocolParametersRaw <- decodeAeson aeson
    prices <- decodePrices ps

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
      , uTxOCostPerWord: Coin ps.coinsPerUtxoByte
      , costModels: Costmdls $ Map.fromFoldable
          [ PlutusV1 /\ convertCostModel ps.costModels."plutus:v1" ]
      , prices: prices
      , maxTxExUnits: Just $ decodeExUnits ps.maxExecutionUnitsPerTransaction
      , maxBlockExUnits: Just $ decodeExUnits ps.maxExecutionUnitsPerBlock
      , maxValueSize: ps.maxValueSize
      , collateralPercent: ps.collateralPercentage
      , maxCollateralInputs: ps.maxCollateralInputs
      }
    where
    decodeExUnits
      :: { memory :: BigInt, steps :: BigInt } -> ExUnits
    decodeExUnits { memory, steps } = { mem: memory, steps }

    decodePrices
      :: ProtocolParametersRaw -> Either JsonDecodeError (Maybe ExUnitPrices)
    decodePrices ps = note (TypeMismatch "ExUnitPrices") do
      memPrice <- rationalToSubcoin ps.prices.memory
      stepPrice <- rationalToSubcoin ps.prices.steps
      pure $ Just { memPrice, stepPrice } -- Maybe ExUnits

-- | A type that represents a JSON-encoded Costmodel in format used by Ogmios
type CostModel =
  { "addInteger-cpu-arguments-intercept" :: Int
  , "addInteger-cpu-arguments-slope" :: Int
  , "addInteger-memory-arguments-intercept" :: Int
  , "addInteger-memory-arguments-slope" :: Int
  , "appendByteString-cpu-arguments-intercept" :: Int
  , "appendByteString-cpu-arguments-slope" :: Int
  , "appendByteString-memory-arguments-intercept" :: Int
  , "appendByteString-memory-arguments-slope" :: Int
  , "appendString-cpu-arguments-intercept" :: Int
  , "appendString-cpu-arguments-slope" :: Int
  , "appendString-memory-arguments-intercept" :: Int
  , "appendString-memory-arguments-slope" :: Int
  , "bData-cpu-arguments" :: Int
  , "bData-memory-arguments" :: Int
  , "blake2b_256-cpu-arguments-intercept" :: Int
  , "blake2b_256-cpu-arguments-slope" :: Int
  , "blake2b_256-memory-arguments" :: Int
  , "cekApplyCost-exBudgetCPU" :: Int
  , "cekApplyCost-exBudgetMemory" :: Int
  , "cekBuiltinCost-exBudgetCPU" :: Int
  , "cekBuiltinCost-exBudgetMemory" :: Int
  , "cekConstCost-exBudgetCPU" :: Int
  , "cekConstCost-exBudgetMemory" :: Int
  , "cekDelayCost-exBudgetCPU" :: Int
  , "cekDelayCost-exBudgetMemory" :: Int
  , "cekForceCost-exBudgetCPU" :: Int
  , "cekForceCost-exBudgetMemory" :: Int
  , "cekLamCost-exBudgetCPU" :: Int
  , "cekLamCost-exBudgetMemory" :: Int
  , "cekStartupCost-exBudgetCPU" :: Int
  , "cekStartupCost-exBudgetMemory" :: Int
  , "cekVarCost-exBudgetCPU" :: Int
  , "cekVarCost-exBudgetMemory" :: Int
  , "chooseData-cpu-arguments" :: Int
  , "chooseData-memory-arguments" :: Int
  , "chooseList-cpu-arguments" :: Int
  , "chooseList-memory-arguments" :: Int
  , "chooseUnit-cpu-arguments" :: Int
  , "chooseUnit-memory-arguments" :: Int
  , "consByteString-cpu-arguments-intercept" :: Int
  , "consByteString-cpu-arguments-slope" :: Int
  , "consByteString-memory-arguments-intercept" :: Int
  , "consByteString-memory-arguments-slope" :: Int
  , "constrData-cpu-arguments" :: Int
  , "constrData-memory-arguments" :: Int
  , "decodeUtf8-cpu-arguments-intercept" :: Int
  , "decodeUtf8-cpu-arguments-slope" :: Int
  , "decodeUtf8-memory-arguments-intercept" :: Int
  , "decodeUtf8-memory-arguments-slope" :: Int
  , "divideInteger-cpu-arguments-constant" :: Int
  , "divideInteger-cpu-arguments-model-arguments-intercept" :: Int
  , "divideInteger-cpu-arguments-model-arguments-slope" :: Int
  , "divideInteger-memory-arguments-intercept" :: Int
  , "divideInteger-memory-arguments-minimum" :: Int
  , "divideInteger-memory-arguments-slope" :: Int
  , "encodeUtf8-cpu-arguments-intercept" :: Int
  , "encodeUtf8-cpu-arguments-slope" :: Int
  , "encodeUtf8-memory-arguments-intercept" :: Int
  , "encodeUtf8-memory-arguments-slope" :: Int
  , "equalsByteString-cpu-arguments-constant" :: Int
  , "equalsByteString-cpu-arguments-intercept" :: Int
  , "equalsByteString-cpu-arguments-slope" :: Int
  , "equalsByteString-memory-arguments" :: Int
  , "equalsData-cpu-arguments-intercept" :: Int
  , "equalsData-cpu-arguments-slope" :: Int
  , "equalsData-memory-arguments" :: Int
  , "equalsInteger-cpu-arguments-intercept" :: Int
  , "equalsInteger-cpu-arguments-slope" :: Int
  , "equalsInteger-memory-arguments" :: Int
  , "equalsString-cpu-arguments-constant" :: Int
  , "equalsString-cpu-arguments-intercept" :: Int
  , "equalsString-cpu-arguments-slope" :: Int
  , "equalsString-memory-arguments" :: Int
  , "fstPair-cpu-arguments" :: Int
  , "fstPair-memory-arguments" :: Int
  , "headList-cpu-arguments" :: Int
  , "headList-memory-arguments" :: Int
  , "iData-cpu-arguments" :: Int
  , "iData-memory-arguments" :: Int
  , "ifThenElse-cpu-arguments" :: Int
  , "ifThenElse-memory-arguments" :: Int
  , "indexByteString-cpu-arguments" :: Int
  , "indexByteString-memory-arguments" :: Int
  , "lengthOfByteString-cpu-arguments" :: Int
  , "lengthOfByteString-memory-arguments" :: Int
  , "lessThanByteString-cpu-arguments-intercept" :: Int
  , "lessThanByteString-cpu-arguments-slope" :: Int
  , "lessThanByteString-memory-arguments" :: Int
  , "lessThanEqualsByteString-cpu-arguments-intercept" :: Int
  , "lessThanEqualsByteString-cpu-arguments-slope" :: Int
  , "lessThanEqualsByteString-memory-arguments" :: Int
  , "lessThanEqualsInteger-cpu-arguments-intercept" :: Int
  , "lessThanEqualsInteger-cpu-arguments-slope" :: Int
  , "lessThanEqualsInteger-memory-arguments" :: Int
  , "lessThanInteger-cpu-arguments-intercept" :: Int
  , "lessThanInteger-cpu-arguments-slope" :: Int
  , "lessThanInteger-memory-arguments" :: Int
  , "listData-cpu-arguments" :: Int
  , "listData-memory-arguments" :: Int
  , "mapData-cpu-arguments" :: Int
  , "mapData-memory-arguments" :: Int
  , "mkCons-cpu-arguments" :: Int
  , "mkCons-memory-arguments" :: Int
  , "mkNilData-cpu-arguments" :: Int
  , "mkNilData-memory-arguments" :: Int
  , "mkNilPairData-cpu-arguments" :: Int
  , "mkNilPairData-memory-arguments" :: Int
  , "mkPairData-cpu-arguments" :: Int
  , "mkPairData-memory-arguments" :: Int
  , "modInteger-cpu-arguments-constant" :: Int
  , "modInteger-cpu-arguments-model-arguments-intercept" :: Int
  , "modInteger-cpu-arguments-model-arguments-slope" :: Int
  , "modInteger-memory-arguments-intercept" :: Int
  , "modInteger-memory-arguments-minimum" :: Int
  , "modInteger-memory-arguments-slope" :: Int
  , "multiplyInteger-cpu-arguments-intercept" :: Int
  , "multiplyInteger-cpu-arguments-slope" :: Int
  , "multiplyInteger-memory-arguments-intercept" :: Int
  , "multiplyInteger-memory-arguments-slope" :: Int
  , "nullList-cpu-arguments" :: Int
  , "nullList-memory-arguments" :: Int
  , "quotientInteger-cpu-arguments-constant" :: Int
  , "quotientInteger-cpu-arguments-model-arguments-intercept" :: Int
  , "quotientInteger-cpu-arguments-model-arguments-slope" :: Int
  , "quotientInteger-memory-arguments-intercept" :: Int
  , "quotientInteger-memory-arguments-minimum" :: Int
  , "quotientInteger-memory-arguments-slope" :: Int
  , "remainderInteger-cpu-arguments-constant" :: Int
  , "remainderInteger-cpu-arguments-model-arguments-intercept" :: Int
  , "remainderInteger-cpu-arguments-model-arguments-slope" :: Int
  , "remainderInteger-memory-arguments-intercept" :: Int
  , "remainderInteger-memory-arguments-minimum" :: Int
  , "remainderInteger-memory-arguments-slope" :: Int
  , "sha2_256-cpu-arguments-intercept" :: Int
  , "sha2_256-cpu-arguments-slope" :: Int
  , "sha2_256-memory-arguments" :: Int
  , "sha3_256-cpu-arguments-intercept" :: Int
  , "sha3_256-cpu-arguments-slope" :: Int
  , "sha3_256-memory-arguments" :: Int
  , "sliceByteString-cpu-arguments-intercept" :: Int
  , "sliceByteString-cpu-arguments-slope" :: Int
  , "sliceByteString-memory-arguments-intercept" :: Int
  , "sliceByteString-memory-arguments-slope" :: Int
  , "sndPair-cpu-arguments" :: Int
  , "sndPair-memory-arguments" :: Int
  , "subtractInteger-cpu-arguments-intercept" :: Int
  , "subtractInteger-cpu-arguments-slope" :: Int
  , "subtractInteger-memory-arguments-intercept" :: Int
  , "subtractInteger-memory-arguments-slope" :: Int
  , "tailList-cpu-arguments" :: Int
  , "tailList-memory-arguments" :: Int
  , "trace-cpu-arguments" :: Int
  , "trace-memory-arguments" :: Int
  , "unBData-cpu-arguments" :: Int
  , "unBData-memory-arguments" :: Int
  , "unConstrData-cpu-arguments" :: Int
  , "unConstrData-memory-arguments" :: Int
  , "unIData-cpu-arguments" :: Int
  , "unIData-memory-arguments" :: Int
  , "unListData-cpu-arguments" :: Int
  , "unListData-memory-arguments" :: Int
  , "unMapData-cpu-arguments" :: Int
  , "unMapData-memory-arguments" :: Int
  , "verifyEd25519Signature-cpu-arguments-intercept" :: Int
  , "verifyEd25519Signature-cpu-arguments-slope" :: Int
  , "verifyEd25519Signature-memory-arguments" :: Int
  }

convertCostModel :: CostModel -> T.CostModel
convertCostModel model = wrap
  [ model."addInteger-cpu-arguments-intercept"
  , model."addInteger-cpu-arguments-slope"
  , model."addInteger-memory-arguments-intercept"
  , model."addInteger-memory-arguments-slope"
  , model."appendByteString-cpu-arguments-intercept"
  , model."appendByteString-cpu-arguments-slope"
  , model."appendByteString-memory-arguments-intercept"
  , model."appendByteString-memory-arguments-slope"
  , model."appendString-cpu-arguments-intercept"
  , model."appendString-cpu-arguments-slope"
  , model."appendString-memory-arguments-intercept"
  , model."appendString-memory-arguments-slope"
  , model."bData-cpu-arguments"
  , model."bData-memory-arguments"
  , model."blake2b_256-cpu-arguments-intercept"
  , model."blake2b_256-cpu-arguments-slope"
  , model."blake2b_256-memory-arguments"
  , model."cekApplyCost-exBudgetCPU"
  , model."cekApplyCost-exBudgetMemory"
  , model."cekBuiltinCost-exBudgetCPU"
  , model."cekBuiltinCost-exBudgetMemory"
  , model."cekConstCost-exBudgetCPU"
  , model."cekConstCost-exBudgetMemory"
  , model."cekDelayCost-exBudgetCPU"
  , model."cekDelayCost-exBudgetMemory"
  , model."cekForceCost-exBudgetCPU"
  , model."cekForceCost-exBudgetMemory"
  , model."cekLamCost-exBudgetCPU"
  , model."cekLamCost-exBudgetMemory"
  , model."cekStartupCost-exBudgetCPU"
  , model."cekStartupCost-exBudgetMemory"
  , model."cekVarCost-exBudgetCPU"
  , model."cekVarCost-exBudgetMemory"
  , model."chooseData-cpu-arguments"
  , model."chooseData-memory-arguments"
  , model."chooseList-cpu-arguments"
  , model."chooseList-memory-arguments"
  , model."chooseUnit-cpu-arguments"
  , model."chooseUnit-memory-arguments"
  , model."consByteString-cpu-arguments-intercept"
  , model."consByteString-cpu-arguments-slope"
  , model."consByteString-memory-arguments-intercept"
  , model."consByteString-memory-arguments-slope"
  , model."constrData-cpu-arguments"
  , model."constrData-memory-arguments"
  , model."decodeUtf8-cpu-arguments-intercept"
  , model."decodeUtf8-cpu-arguments-slope"
  , model."decodeUtf8-memory-arguments-intercept"
  , model."decodeUtf8-memory-arguments-slope"
  , model."divideInteger-cpu-arguments-constant"
  , model."divideInteger-cpu-arguments-model-arguments-intercept"
  , model."divideInteger-cpu-arguments-model-arguments-slope"
  , model."divideInteger-memory-arguments-intercept"
  , model."divideInteger-memory-arguments-minimum"
  , model."divideInteger-memory-arguments-slope"
  , model."encodeUtf8-cpu-arguments-intercept"
  , model."encodeUtf8-cpu-arguments-slope"
  , model."encodeUtf8-memory-arguments-intercept"
  , model."encodeUtf8-memory-arguments-slope"
  , model."equalsByteString-cpu-arguments-constant"
  , model."equalsByteString-cpu-arguments-intercept"
  , model."equalsByteString-cpu-arguments-slope"
  , model."equalsByteString-memory-arguments"
  , model."equalsData-cpu-arguments-intercept"
  , model."equalsData-cpu-arguments-slope"
  , model."equalsData-memory-arguments"
  , model."equalsInteger-cpu-arguments-intercept"
  , model."equalsInteger-cpu-arguments-slope"
  , model."equalsInteger-memory-arguments"
  , model."equalsString-cpu-arguments-constant"
  , model."equalsString-cpu-arguments-intercept"
  , model."equalsString-cpu-arguments-slope"
  , model."equalsString-memory-arguments"
  , model."fstPair-cpu-arguments"
  , model."fstPair-memory-arguments"
  , model."headList-cpu-arguments"
  , model."headList-memory-arguments"
  , model."iData-cpu-arguments"
  , model."iData-memory-arguments"
  , model."ifThenElse-cpu-arguments"
  , model."ifThenElse-memory-arguments"
  , model."indexByteString-cpu-arguments"
  , model."indexByteString-memory-arguments"
  , model."lengthOfByteString-cpu-arguments"
  , model."lengthOfByteString-memory-arguments"
  , model."lessThanByteString-cpu-arguments-intercept"
  , model."lessThanByteString-cpu-arguments-slope"
  , model."lessThanByteString-memory-arguments"
  , model."lessThanEqualsByteString-cpu-arguments-intercept"
  , model."lessThanEqualsByteString-cpu-arguments-slope"
  , model."lessThanEqualsByteString-memory-arguments"
  , model."lessThanEqualsInteger-cpu-arguments-intercept"
  , model."lessThanEqualsInteger-cpu-arguments-slope"
  , model."lessThanEqualsInteger-memory-arguments"
  , model."lessThanInteger-cpu-arguments-intercept"
  , model."lessThanInteger-cpu-arguments-slope"
  , model."lessThanInteger-memory-arguments"
  , model."listData-cpu-arguments"
  , model."listData-memory-arguments"
  , model."mapData-cpu-arguments"
  , model."mapData-memory-arguments"
  , model."mkCons-cpu-arguments"
  , model."mkCons-memory-arguments"
  , model."mkNilData-cpu-arguments"
  , model."mkNilData-memory-arguments"
  , model."mkNilPairData-cpu-arguments"
  , model."mkNilPairData-memory-arguments"
  , model."mkPairData-cpu-arguments"
  , model."mkPairData-memory-arguments"
  , model."modInteger-cpu-arguments-constant"
  , model."modInteger-cpu-arguments-model-arguments-intercept"
  , model."modInteger-cpu-arguments-model-arguments-slope"
  , model."modInteger-memory-arguments-intercept"
  , model."modInteger-memory-arguments-minimum"
  , model."modInteger-memory-arguments-slope"
  , model."multiplyInteger-cpu-arguments-intercept"
  , model."multiplyInteger-cpu-arguments-slope"
  , model."multiplyInteger-memory-arguments-intercept"
  , model."multiplyInteger-memory-arguments-slope"
  , model."nullList-cpu-arguments"
  , model."nullList-memory-arguments"
  , model."quotientInteger-cpu-arguments-constant"
  , model."quotientInteger-cpu-arguments-model-arguments-intercept"
  , model."quotientInteger-cpu-arguments-model-arguments-slope"
  , model."quotientInteger-memory-arguments-intercept"
  , model."quotientInteger-memory-arguments-minimum"
  , model."quotientInteger-memory-arguments-slope"
  , model."remainderInteger-cpu-arguments-constant"
  , model."remainderInteger-cpu-arguments-model-arguments-intercept"
  , model."remainderInteger-cpu-arguments-model-arguments-slope"
  , model."remainderInteger-memory-arguments-intercept"
  , model."remainderInteger-memory-arguments-minimum"
  , model."remainderInteger-memory-arguments-slope"
  , model."sha2_256-cpu-arguments-intercept"
  , model."sha2_256-cpu-arguments-slope"
  , model."sha2_256-memory-arguments"
  , model."sha3_256-cpu-arguments-intercept"
  , model."sha3_256-cpu-arguments-slope"
  , model."sha3_256-memory-arguments"
  , model."sliceByteString-cpu-arguments-intercept"
  , model."sliceByteString-cpu-arguments-slope"
  , model."sliceByteString-memory-arguments-intercept"
  , model."sliceByteString-memory-arguments-slope"
  , model."sndPair-cpu-arguments"
  , model."sndPair-memory-arguments"
  , model."subtractInteger-cpu-arguments-intercept"
  , model."subtractInteger-cpu-arguments-slope"
  , model."subtractInteger-memory-arguments-intercept"
  , model."subtractInteger-memory-arguments-slope"
  , model."tailList-cpu-arguments"
  , model."tailList-memory-arguments"
  , model."trace-cpu-arguments"
  , model."trace-memory-arguments"
  , model."unBData-cpu-arguments"
  , model."unBData-memory-arguments"
  , model."unConstrData-cpu-arguments"
  , model."unConstrData-memory-arguments"
  , model."unIData-cpu-arguments"
  , model."unIData-memory-arguments"
  , model."unListData-cpu-arguments"
  , model."unListData-memory-arguments"
  , model."unMapData-cpu-arguments"
  , model."unMapData-memory-arguments"
  , model."verifyEd25519Signature-cpu-arguments-intercept"
  , model."verifyEd25519Signature-cpu-arguments-slope"
  , model."verifyEd25519Signature-memory-arguments"
  ]

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
  txId <- getField o "txId"
  index <- getField o "index"
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
  address <- getField o "address"
  value <- parseValue o
  let datum = hush $ getField o "datum"
  pure $ { address, value, datum }

-- parses the `Value` type
parseValue :: Object Aeson -> Either JsonDecodeError Value
parseValue outer = do
  o <- getField outer "value"
  coins <- getField o "coins"
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
