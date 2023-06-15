-- | Provides types and instances to create Ogmios requests and decode
-- | its responses.
module Ctl.Internal.QueryM.Ogmios
  ( ChainOrigin(ChainOrigin)
  , ChainPoint
  , ChainTipQR(CtChainOrigin, CtChainPoint)
  , CurrentEpoch(CurrentEpoch)
  , DelegationsAndRewardsR(DelegationsAndRewardsR)
  , ExecutionUnits
  , MempoolSizeAndCapacity(MempoolSizeAndCapacity)
  , MempoolSnapshotAcquired
  , MempoolTransaction(MempoolTransaction)
  , OgmiosAddress
  , OgmiosBlockHeaderHash(OgmiosBlockHeaderHash)
  , OgmiosTxOut
  , OgmiosTxOutRef
  , PParamRational(PParamRational)
  , PoolParameters
  , PoolParametersR(PoolParametersR)
  , OgmiosProtocolParameters(OgmiosProtocolParameters)
  , RedeemerPointer
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
  , OgmiosEraSummaries(OgmiosEraSummaries)
  , OgmiosScript
  , OgmiosSystemStart(OgmiosSystemStart)
  , OgmiosTxIn
  , OgmiosTxId
  , SubmitTxR(SubmitTxSuccess, SubmitFail)
  , TxEvaluationFailure(UnparsedError, ScriptFailures)
  , TxEvaluationResult(TxEvaluationResult)
  , TxEvaluationR(TxEvaluationR)
  , PoolIdsR
  , TxHash
  , UtxoQR(UtxoQR)
  , UtxoQueryResult
  , acquireMempoolSnapshotCall
  , aesonArray
  , aesonObject
  , evaluateTxCall
  , queryPoolIdsCall
  , mempoolSnapshotHasTxCall
  , mempoolSnapshotNextTxCall
  , mempoolSnpashotSizeAndCapacityCall
  , mkOgmiosCallType
  , queryChainTipCall
  , queryCurrentEpochCall
  , queryEraSummariesCall
  , queryProtocolParametersCall
  , querySystemStartCall
  , queryPoolParameters
  , queryDelegationsAndRewards
  , releaseMempoolCall
  , submitTxCall
  , slotLengthFactor
  , parseIpv6String
  , rationalToSubcoin
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch, MissingValue, AtKey)
  , caseAesonArray
  , caseAesonObject
  , caseAesonString
  , decodeAeson
  , encodeAeson
  , fromArray
  , getField
  , getFieldOptional
  , getFieldOptional'
  , isNull
  , isString
  , stringifyAeson
  , toString
  , (.:)
  , (.:?)
  )
import Control.Alt ((<|>))
import Control.Alternative (guard)
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
  , Ipv4(Ipv4)
  , Ipv6(Ipv6)
  , PoolMetadata(PoolMetadata)
  , PoolMetadataHash(PoolMetadataHash)
  , PoolPubKeyHash
  , Relay(MultiHostName, SingleHostAddr, SingleHostName)
  , SubCoin
  , URL(URL)
  , UnitInterval
  )
import Ctl.Internal.Cardano.Types.Value
  ( Coin(Coin)
  , CurrencySymbol
  , NonAdaAsset
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
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Helpers (encodeMap, showWithParens)
import Ctl.Internal.QueryM.JsonWsp (JsonWspCall, JsonWspRequest, mkCallType)
import Ctl.Internal.Serialization.Address (Slot(Slot))
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash, ed25519KeyHashFromBytes)
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.BigNum (fromBigInt, fromString) as BigNum
import Ctl.Internal.Types.ByteArray
  ( ByteArray
  , byteArrayFromIntArray
  , byteArrayToHex
  , hexToByteArray
  )
import Ctl.Internal.Types.CborBytes (CborBytes, cborBytesToHex)
import Ctl.Internal.Types.Epoch (Epoch(Epoch))
import Ctl.Internal.Types.EraSummaries
  ( EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  )
import Ctl.Internal.Types.Natural (Natural)
import Ctl.Internal.Types.Natural (fromString) as Natural
import Ctl.Internal.Types.ProtocolParameters
  ( CoinsPerUtxoUnit(CoinsPerUtxoWord, CoinsPerUtxoByte)
  , CostModelV1
  , CostModelV2
  , ProtocolParameters(ProtocolParameters)
  , convertPlutusV1CostModel
  , convertPlutusV2CostModel
  )
import Ctl.Internal.Types.Rational (Rational, (%))
import Ctl.Internal.Types.Rational as Rational
import Ctl.Internal.Types.RedeemerTag (RedeemerTag)
import Ctl.Internal.Types.RedeemerTag (fromString) as RedeemerTag
import Ctl.Internal.Types.RewardAddress (RewardAddress)
import Ctl.Internal.Types.Scripts
  ( Language(PlutusV1, PlutusV2)
  , PlutusScript(PlutusScript)
  )
import Ctl.Internal.Types.SystemStart
  ( SystemStart
  , sysStartFromOgmiosTimestamp
  , sysStartToOgmiosTimestamp
  )
import Ctl.Internal.Types.TokenName (TokenName, getTokenName, mkTokenName)
import Ctl.Internal.Types.VRFKeyHash (VRFKeyHash(VRFKeyHash))
import Data.Array (catMaybes, index)
import Data.Array (head, length, replicate) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), either, note)
import Data.Foldable (fold, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String
  ( Pattern(Pattern)
  , Replacement(Replacement)
  , indexOf
  , split
  , splitAt
  , uncons
  )
import Data.String (replaceAll) as String
import Data.String.Common (split) as String
import Data.String.Utils as StringUtils
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (Tuple(Tuple), snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Foreign.Object (Object)
import Foreign.Object (singleton, toUnfoldable) as ForeignObject
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (type (|+|), toEither1)

--------------------------------------------------------------------------------
-- Local State Query Protocol
-- https://ogmios.dev/mini-protocols/local-state-query/
--------------------------------------------------------------------------------

-- | Queries Ogmios for the system start Datetime
querySystemStartCall :: JsonWspCall Unit OgmiosSystemStart
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
queryEraSummariesCall :: JsonWspCall Unit OgmiosEraSummaries
queryEraSummariesCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "eraSummaries" }
  }

-- | Queries Ogmios for the current protocol parameters
queryProtocolParametersCall :: JsonWspCall Unit OgmiosProtocolParameters
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

queryPoolIdsCall :: JsonWspCall Unit PoolIdsR
queryPoolIdsCall = mkOgmiosCallType
  { methodname: "Query"
  , args: const { query: "poolIds" }
  }

queryPoolParameters :: JsonWspCall (Array PoolPubKeyHash) PoolParametersR
queryPoolParameters = mkOgmiosCallType
  { methodname: "Query"
  , args: \params -> { query: { poolParameters: params } }
  }

queryDelegationsAndRewards :: JsonWspCall (Array String) DelegationsAndRewardsR
queryDelegationsAndRewards = mkOgmiosCallType
  { methodname: "Query"
  , args: \skhs ->
      { query:
          { delegationsAndRewards: skhs
          }
      }
  }

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

mempoolSnapshotNextTxCall
  :: MempoolSnapshotAcquired -> JsonWspCall Unit (Maybe MempoolTransaction)
mempoolSnapshotNextTxCall _ = mkOgmiosCallType
  { methodname: "NextTx"
  , args: const { fields: "all" }
  }

mempoolSnpashotSizeAndCapacityCall
  :: MempoolSnapshotAcquired -> JsonWspCall Unit MempoolSizeAndCapacity
mempoolSnpashotSizeAndCapacityCall _ =
  mkOgmiosCallTypeNoArgs "SizeAndCapacity"

releaseMempoolCall
  :: MempoolSnapshotAcquired -> JsonWspCall Unit String
releaseMempoolCall _ =
  mkOgmiosCallTypeNoArgs "ReleaseMempool"

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

-- | The acquired snapshot’s size (in bytes), number of transactions, and capacity
-- | (in bytes).
newtype MempoolSizeAndCapacity = MempoolSizeAndCapacity
  { capacity :: Int
  , currentSize :: Int
  , numberOfTxs :: Int
  }

derive instance Generic MempoolSizeAndCapacity _
derive instance Newtype MempoolSizeAndCapacity _

instance Show MempoolSizeAndCapacity where
  show = genericShow

instance DecodeAeson MempoolSizeAndCapacity where
  decodeAeson = aesonObject \o -> do
    capacity <- getField o "capacity"
    currentSize <- getField o "currentSize"
    numberOfTxs <- getField o "numberOfTxs"

    pure $ wrap { capacity, currentSize, numberOfTxs }

newtype MempoolTransaction = MempoolTransaction
  { id :: OgmiosTxId
  , raw :: String
  }

derive instance Generic MempoolTransaction _
derive instance Newtype MempoolTransaction _

instance Show MempoolTransaction where
  show = genericShow

instance DecodeAeson MempoolTransaction where
  decodeAeson = aesonObject \o -> do
    id <- o .: "id"
    raw <- o .: "raw"
    pure $ MempoolTransaction { id, raw }

type OgmiosRewardAddress = String

type OgmiosRequiredSigner = String

newtype OgmiosRedeemer = OgmiosRedeemer
  { redeemer :: String, executionUnits :: ExUnits }

derive instance Generic OgmiosRedeemer _
derive instance Newtype OgmiosRedeemer _

instance Show OgmiosRedeemer where
  show = genericShow

instance DecodeAeson OgmiosRedeemer where
  decodeAeson = aesonObject \o -> do
    redeemer <- o .: "redeemer"
    execUnitsAeson <- o .: "executionUnits"
    executionUnits <- flip aesonObject execUnitsAeson \obj -> do
      mem <- obj .: "memory"
      steps <- obj .: "steps"
      pure { mem, steps }
    pure $ OgmiosRedeemer { redeemer, executionUnits }

newtype Update = Update
  { epoch :: UInt
  , proposal :: Proposal
  }

derive instance Generic Update _
derive instance Newtype Update _
instance Show Update where
  show = genericShow

instance DecodeAeson Update where
  decodeAeson = aesonObject \o -> do
    epoch <- o .: "epoch"
    proposal <- o .: "proposal"
    pure $ Update { epoch, proposal }

newtype Proposal = Proposal (Map String OgmiosProtocolParameters)

derive instance Generic Proposal _
derive instance Newtype Proposal _

instance Show Proposal where
  show = genericShow

instance DecodeAeson Proposal where
  decodeAeson = aesonObject $ \obj -> do
    let proposalList = ForeignObject.toUnfoldable obj :: Array (String /\ Aeson)
    Proposal <<< Map.fromFoldable <$>
      traverse decodeWithdrawlAmount proposalList
    where
    decodeWithdrawlAmount
      :: String /\ Aeson
      -> Either JsonDecodeError (String /\ OgmiosProtocolParameters)
    decodeWithdrawlAmount (string /\ aeson) = do
      opp <- decodeAeson aeson
      pure $ string /\ opp

type Nonce = String

type Ratio = String

newtype ProtocolVersion = ProtocolVersion
  { major :: UInt
  , minor :: UInt
  , patch :: Maybe UInt
  }

derive instance Generic ProtocolVersion _
derive instance Newtype ProtocolVersion _

instance Show ProtocolVersion where
  show = genericShow

instance DecodeAeson ProtocolVersion where
  decodeAeson = aesonObject $ \o -> do
    major <- o .: "major"
    minor <- o .: "minor"
    patch <- o .:? "patch"
    pure $ ProtocolVersion { major, minor, patch }

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
newtype OgmiosSystemStart = OgmiosSystemStart SystemStart

derive instance Generic OgmiosSystemStart _
derive instance Newtype OgmiosSystemStart _
derive newtype instance Eq OgmiosSystemStart

instance Show OgmiosSystemStart where
  show = genericShow

instance DecodeAeson OgmiosSystemStart where
  decodeAeson =
    caseAesonString (Left (TypeMismatch "Timestamp string"))
      (map wrap <<< lmap TypeMismatch <<< sysStartFromOgmiosTimestamp)

instance EncodeAeson OgmiosSystemStart where
  encodeAeson = encodeAeson <<< sysStartToOgmiosTimestamp <<< unwrap

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

newtype OgmiosEraSummaries = OgmiosEraSummaries EraSummaries

derive instance Generic OgmiosEraSummaries _
derive instance Newtype OgmiosEraSummaries _
derive newtype instance Eq OgmiosEraSummaries

instance Show OgmiosEraSummaries where
  show = genericShow

instance DecodeAeson OgmiosEraSummaries where
  decodeAeson = aesonArray (map (wrap <<< wrap) <<< traverse decodeEraSummary)
    where
    decodeEraSummary :: Aeson -> Either JsonDecodeError EraSummary
    decodeEraSummary = aesonObject \o -> do
      start <- getField o "start"
      -- The field "end" is required by Ogmios API, but it can optionally return
      -- Null, so we want to fail if the field is absent but make Null value
      -- acceptable in presence of the field (hence why "end" is wrapped in
      -- `Maybe`).
      end' <- getField o "end"
      end <- if isNull end' then pure Nothing else Just <$> decodeAeson end'
      parameters <- decodeEraSummaryParameters =<< getField o "parameters"
      pure $ wrap { start, end, parameters }

    decodeEraSummaryParameters
      :: Object Aeson -> Either JsonDecodeError EraSummaryParameters
    decodeEraSummaryParameters o = do
      epochLength <- getField o "epochLength"
      slotLength <- wrap <$> ((*) slotLengthFactor <$> getField o "slotLength")
      safeZone <- fromMaybe zero <$> getField o "safeZone"
      pure $ wrap { epochLength, slotLength, safeZone }

instance EncodeAeson OgmiosEraSummaries where
  encodeAeson (OgmiosEraSummaries (EraSummaries eraSummaries)) =
    fromArray $ map encodeEraSummary eraSummaries
    where
    encodeEraSummary :: EraSummary -> Aeson
    encodeEraSummary (EraSummary { start, end, parameters }) =
      encodeAeson
        { "start": start
        , "end": end
        , "parameters": encodeEraSummaryParameters parameters
        }

    encodeEraSummaryParameters :: EraSummaryParameters -> Aeson
    encodeEraSummaryParameters (EraSummaryParameters params) =
      encodeAeson
        { "epochLength": params.epochLength
        , "slotLength": params.slotLength
        , "safeZone": params.safeZone
        }

-- Ogmios returns `slotLength` in seconds, and we use milliseconds,
-- so we need to convert between them.
slotLengthFactor :: Number
slotLengthFactor = 1000.0

---------------- DELEGATIONS & REWARDS QUERY RESPONSE & PARSING

newtype DelegationsAndRewardsR = DelegationsAndRewardsR
  ( Map String
      { rewards :: Maybe Coin
      , delegate :: Maybe PoolPubKeyHash
      }
  )

derive instance Generic DelegationsAndRewardsR _
derive instance Newtype DelegationsAndRewardsR _

instance DecodeAeson DelegationsAndRewardsR where
  decodeAeson aeson = do
    obj :: Object (Object Aeson) <- decodeAeson aeson
    kvs <- for (Object.toUnfoldable obj :: Array _) \(Tuple k objParams) -> do
      rewards <- map Coin <$> objParams .:? "rewards"
      delegate <- objParams .:? "delegate"
      pure $ k /\ { rewards, delegate }
    pure $ DelegationsAndRewardsR $ Map.fromFoldable kvs

---------------- POOL PARAMETERS QUERY RESPONSE & PARSING

type PoolParameters =
  { vrfKeyhash :: VRFKeyHash
  -- needed to prove that the pool won the lottery
  , pledge :: BigNum
  , cost :: BigNum -- >= pparams.minPoolCost
  , margin :: UnitInterval -- proportion that goes to the reward account
  , rewardAccount :: RewardAddress
  , poolOwners :: Array Ed25519KeyHash
  -- payment key hashes that contribute to pledge amount
  , relays :: Array Relay
  , poolMetadata :: Maybe PoolMetadata
  }

newtype PoolParametersR = PoolParametersR (Map String PoolParameters)

derive instance Newtype PoolParametersR _
derive instance Generic PoolParametersR _

instance Show PoolParametersR where
  show = genericShow

instance DecodeAeson PoolParametersR where
  decodeAeson aeson = do
    obj :: Object (Object Aeson) <- decodeAeson aeson
    kvs <- for (Object.toUnfoldable obj :: Array _) \(Tuple k objParams) -> do
      poolParams <- decodePoolParameters objParams
      pure $ k /\ poolParams
    pure $ PoolParametersR $ Map.fromFoldable kvs

decodePoolParameters :: Object Aeson -> Either JsonDecodeError PoolParameters
decodePoolParameters objParams = do
  vrfKeyhash <- decodeVRFKeyHash =<< objParams .: "vrf"
  pledge <- objParams .: "pledge"
  cost <- objParams .: "cost"
  margin <- decodeUnitInterval =<< objParams .: "margin"
  rewardAccount <- objParams .: "rewardAccount"
  poolOwners <- objParams .: "owners"
  relayArr <- objParams .: "relays"
  relays <- for relayArr decodeRelay
  poolMetadata <- objParams .:? "metadata" >>= traverse decodePoolMetadata
  pure
    { vrfKeyhash
    , pledge
    , cost
    , margin
    , rewardAccount
    , poolOwners
    , relays
    , poolMetadata
    }

decodeVRFKeyHash :: Aeson -> Either JsonDecodeError VRFKeyHash
decodeVRFKeyHash = aesonString $ \vrfKeyhashHex -> do
  vrfKeyhashBytes <- note (TypeMismatch "VRFKeyHash") $ hexToByteArray
    vrfKeyhashHex
  note (TypeMismatch "VRFKeyHash") $ VRFKeyHash <$> fromBytes
    (wrap vrfKeyhashBytes)

decodeUnitInterval :: Aeson -> Either JsonDecodeError UnitInterval
decodeUnitInterval aeson = do
  str <- decodeAeson aeson
  case String.split (Pattern "/") str of
    [ num, den ] -> do
      numerator <- note (TypeMismatch "BigNum") $ BigNum.fromString num
      denominator <- note (TypeMismatch "BigNum") $ BigNum.fromString den
      pure
        { numerator
        , denominator
        }
    _ -> Left $ TypeMismatch "UnitInterval"

decodeIpv4 :: Aeson -> Either JsonDecodeError Ipv4
decodeIpv4 aeson = do
  str <- decodeAeson aeson
  case String.split (Pattern ".") str of
    bs@[ _, _, _, _ ] -> do
      ints <- for bs $
        note (TypeMismatch "Ipv4") <<< Int.fromString
      Ipv4 <$> note (TypeMismatch "Ipv4") (byteArrayFromIntArray ints)
    _ -> Left $ TypeMismatch "Ipv4"

decodeIpv6 :: Aeson -> Either JsonDecodeError Ipv6
decodeIpv6 aeson = do
  decodeAeson aeson >>= parseIpv6String >>> note (TypeMismatch "Ipv6")

parseIpv6String :: String -> Maybe Ipv6
parseIpv6String str = do
  let
    parts = String.split (Pattern ":") str
    partsFixed =
      if Array.length parts < 8 then
        -- Normalize double colon
        -- see https://ipcisco.com/lesson/ipv6-address/
        do
          part <- parts
          if part == "" then
            Array.replicate (8 - Array.length parts + 1) ""
          else
            pure part
      else
        parts
  guard (Array.length partsFixed == 8)
  let
    padded = String.replaceAll (Pattern " ") (Replacement "0") $ fold $
      partsFixed
        <#> StringUtils.padStart 4
  Ipv6 <$> hexToByteArray padded

decodeRelay :: Aeson -> Either JsonDecodeError Relay
decodeRelay aeson = do
  obj <- decodeAeson aeson
  let
    decodeSingleHostAddr = do
      port <- obj .:? "port"
      ipv4 <- obj .:? "ipv4" >>= traverse decodeIpv4
      ipv6 <- obj .:? "ipv6" >>= traverse decodeIpv6
      pure $ SingleHostAddr { port, ipv4, ipv6 }
    decodeSingleHostName = do
      port <- obj .: "port"
      dnsName <- obj .: "hostname"
      pure $ SingleHostName { port, dnsName }
    decodeMultiHostName = do
      dnsName <- obj .: "hostname"
      pure $ MultiHostName { dnsName }
  decodeSingleHostName <|> decodeSingleHostAddr <|> decodeMultiHostName

decodePoolMetadata :: Aeson -> Either JsonDecodeError PoolMetadata
decodePoolMetadata aeson = do
  obj <- decodeAeson aeson
  hash <- obj .: "hash" >>= note (TypeMismatch "PoolMetadataHash")
    <<< map PoolMetadataHash
    <<< hexToByteArray
  url <- obj .: "url" <#> URL
  pure $ PoolMetadata { hash, url }

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
  , "desiredNumberOfPools" :: UInt
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

newtype OgmiosProtocolParameters = OgmiosProtocolParameters ProtocolParameters

derive instance Newtype OgmiosProtocolParameters _
derive instance Generic OgmiosProtocolParameters _
derive instance Eq OgmiosProtocolParameters

instance Show OgmiosProtocolParameters where
  show = genericShow

instance DecodeAeson OgmiosProtocolParameters where
  decodeAeson aeson = do
    ps :: ProtocolParametersRaw <- decodeAeson aeson
    prices <- decodePrices ps
    coinsPerUtxoUnit <-
      maybe
        (Left $ AtKey "coinsPerUtxoByte or coinsPerUtxoWord" $ MissingValue)
        pure
        $ (CoinsPerUtxoByte <<< Coin <$> ps.coinsPerUtxoByte) <|>
            (CoinsPerUtxoWord <<< Coin <$> ps.coinsPerUtxoWord)
    pure $ OgmiosProtocolParameters $ ProtocolParameters
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
      , stakePoolTargetNum: ps.desiredNumberOfPools
      , poolPledgeInfluence: unwrap ps.poolInfluence
      , monetaryExpansion: unwrap ps.monetaryExpansion
      , treasuryCut: unwrap ps.treasuryExpansion -- Rational
      , coinsPerUtxoUnit: coinsPerUtxoUnit
      , costModels: Costmdls $ Map.fromFoldable $ catMaybes
          [ pure
              (PlutusV1 /\ convertPlutusV1CostModel ps.costModels."plutus:v1")
          , (PlutusV2 /\ _) <<< convertPlutusV2CostModel <$>
              ps.costModels."plutus:v2"
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

-- | A Blake2b 32-byte digest of an era-independent block header, serialized as
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
  -- for details on why we lose a negligible amount of precision.
  , hash :: OgmiosBlockHeaderHash
  }

---------------- POOL ID RESPONSE

type PoolIdsR = Array PoolPubKeyHash

---------------- ADDITIONAL UTXO MAP REQUEST

newtype AdditionalUtxoSet = AdditionalUtxoSet OgmiosUtxoMap

derive instance Newtype AdditionalUtxoSet _

derive newtype instance Show AdditionalUtxoSet

type OgmiosUtxoMap = Map OgmiosTxOutRef OgmiosTxOut

instance EncodeAeson AdditionalUtxoSet where
  encodeAeson (AdditionalUtxoSet m) =
    encodeAeson $ encode <$> utxos

    where
    utxos :: Array (OgmiosTxOutRef /\ OgmiosTxOut)
    utxos = Map.toUnfoldable m

    encode :: (OgmiosTxOutRef /\ OgmiosTxOut) -> Aeson
    encode (inp /\ out) = encodeAeson $
      { "txId": inp.txId
      , "index": inp.index
      }
        /\
          { "address": out.address
          , "datumHash": out.datumHash
          , "datum": out.datum
          , "script": encodeScriptRef <$> out.script
          , "value":
              { "coins": out.value # valueToCoin # getLovelace
              , "assets": out.value # getNonAdaAsset # encodeNonAdaAsset
              }
          }

    encodeNativeScript :: NativeScript -> Aeson
    encodeNativeScript (ScriptPubkey s) = encodeAeson s
    encodeNativeScript (ScriptAll ss) =
      encodeAeson { "all": encodeNativeScript <$> ss }
    encodeNativeScript (ScriptAny ss) =
      encodeAeson { "any": encodeNativeScript <$> ss }
    encodeNativeScript (ScriptNOfK n ss) =
      encodeAeson $
        ForeignObject.singleton
          (BigInt.toString $ BigInt.fromInt n)
          (encodeNativeScript <$> ss)
    encodeNativeScript (TimelockStart (Slot n)) = encodeAeson { "startsAt": n }
    encodeNativeScript (TimelockExpiry (Slot n)) = encodeAeson
      { "expiresAt": n }

    encodeScriptRef :: ScriptRef -> Aeson
    encodeScriptRef (NativeScriptRef s) =
      encodeAeson { "native": encodeNativeScript s }
    encodeScriptRef (PlutusScriptRef (PlutusScript (s /\ PlutusV1))) =
      encodeAeson { "plutus:v1": s }
    encodeScriptRef (PlutusScriptRef (PlutusScript (s /\ PlutusV2))) =
      encodeAeson { "plutus:v2": s }

    encodeNonAdaAsset :: NonAdaAsset -> Aeson
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

-- helper for assuming we get an array
aesonArray
  :: forall (a :: Type)
   . (Array Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonArray = caseAesonArray (Left (TypeMismatch "Expected Array"))

-- Helper that decodes a string
aesonString
  :: forall (a :: Type)
   . (String -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonString = caseAesonString (Left (TypeMismatch "Expected String"))

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
  script <- getFieldOptional' o "script" >>= case _ of
    Nothing -> pure Nothing
    Just script -> Just <$> parseScript script
  pure { address, value, datumHash, datum, script }

parseScript :: Object Aeson -> Either JsonDecodeError ScriptRef
parseScript script =
  case Array.head $ ForeignObject.toUnfoldable script of
    Just ("plutus:v1" /\ plutusScript) ->
      parsePlutusScriptWithLang PlutusV1 plutusScript

    Just ("plutus:v2" /\ plutusScript) ->
      parsePlutusScriptWithLang PlutusV2 plutusScript

    Just ("native" /\ nativeScript) ->
      NativeScriptRef <$> parseNativeScript nativeScript

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
          (ed25519KeyHashFromBytes =<< hexToByteArray pubKeyHashHex)

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
