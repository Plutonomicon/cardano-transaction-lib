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
  , OgmiosProtocolParameters(OgmiosProtocolParameters)
  , PParamRational(PParamRational)
  , PoolParameters
  , PoolParametersR(PoolParametersR)
  , RedeemerPointer
  , ReleasedMempool(ReleasedMempool)
  , ScriptFailure
      ( ExtraRedeemers
      , MissingRequiredDatums
      , MissingRequiredScripts
      , ValidatorFailed
      , UnknownInputReferencedByRedeemer
      , NonScriptInputReferencedByRedeemer
      , NoCostModelForLanguage
      , InternalLedgerTypeConversionError
      )
  , AdditionalUtxoSet(AdditionalUtxoSet)
  , OgmiosUtxoMap
  , OgmiosDatum
  , OgmiosEraSummaries(OgmiosEraSummaries)
  , OgmiosScript
  , OgmiosSystemStart(OgmiosSystemStart)
  , OgmiosTxIn
  , OgmiosTxId
  , OgmiosError
  , SubmitTxR(SubmitTxSuccess, SubmitFail)
  , StakePoolsQueryArgument(StakePoolsQueryArgument)
  , TxEvaluationFailure(UnparsedError, ScriptFailures)
  , TxEvaluationResult(TxEvaluationResult)
  , TxEvaluationR(TxEvaluationR)
  , TxHash
  , HasTxR(HasTxR)
  , MaybeMempoolTransaction(MaybeMempoolTransaction)
  , acquireMempoolSnapshotCall
  , aesonArray
  , aesonObject
  , evaluateTxCall
  , queryStakePoolsCall
  , mempoolSnapshotHasTxCall
  , mempoolSnapshotNextTxCall
  , mempoolSnapshotSizeAndCapacityCall
  , mkOgmiosCallType
  , mkOgmiosCallTypeNoArgs
  , queryChainTipCall
  , queryCurrentEpochCall
  , queryEraSummariesCall
  , queryProtocolParametersCall
  , querySystemStartCall
  , queryDelegationsAndRewards
  , releaseMempoolCall
  , submitTxCall
  , submitSuccessPartialResp
  , slotLengthFactor
  , parseIpv6String
  , rationalToSubcoin
  , showRedeemerPointer
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch, UnexpectedValue)
  , caseAesonArray
  , caseAesonObject
  , caseAesonString
  , decodeAeson
  , encodeAeson
  , fromArray
  , fromString
  , getField
  , isNull
  , (.:)
  , (.:?)
  )
import Control.Alt ((<|>))
import Control.Alternative (guard)
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
  ( CostModel(CostModel)
  , Costmdls(Costmdls)
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
  , Value
  , getCurrencySymbol
  , getLovelace
  , getNonAdaAsset
  , unwrapNonAdaAsset
  , valueToCoin
  )
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Helpers (encodeMap, showWithParens)
import Ctl.Internal.QueryM.JsonRpc2
  ( class DecodeOgmios
  , JsonRpc2Call
  , JsonRpc2Request
  , OgmiosDecodeError(DecodingError)
  , decodeError
  , decodeResult
  , mkCallType
  )
import Ctl.Internal.Serialization.Address (Slot(Slot))
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash)
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
import Ctl.Internal.Types.Int as Csl
import Ctl.Internal.Types.Natural (Natural)
import Ctl.Internal.Types.Natural (fromString) as Natural
import Ctl.Internal.Types.ProtocolParameters
  ( CoinsPerUtxoUnit(CoinsPerUtxoByte)
  , ProtocolParameters(ProtocolParameters)
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
import Ctl.Internal.Types.TokenName (getTokenName)
import Ctl.Internal.Types.VRFKeyHash (VRFKeyHash(VRFKeyHash))
import Data.Argonaut.Encode.Encoders as Argonaut
import Data.Array (catMaybes)
import Data.Array (fromFoldable, length, replicate) as Array
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), either, note)
import Data.Foldable (fold, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as Int
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern), Replacement(Replacement), split)
import Data.String (replaceAll) as String
import Data.String.Common (split) as String
import Data.String.Utils as StringUtils
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Foreign.Object (Object)
import Foreign.Object as Object
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (type (|+|), toEither1)

--------------------------------------------------------------------------------
-- Local State Query Protocol
-- https://ogmios.dev/mini-protocols/local-state-query/
--------------------------------------------------------------------------------

-- | Queries Ogmios for the system start Datetime
querySystemStartCall :: JsonRpc2Call Unit OgmiosSystemStart
querySystemStartCall = mkOgmiosCallTypeNoArgs "queryNetwork/startTime"

-- | Queries Ogmios for the current epoch
queryCurrentEpochCall :: JsonRpc2Call Unit CurrentEpoch
queryCurrentEpochCall = mkOgmiosCallTypeNoArgs "queryLedgerState/epoch"

-- | Queries Ogmios for an array of era summaries, used for Slot arithmetic.
queryEraSummariesCall :: JsonRpc2Call Unit OgmiosEraSummaries
queryEraSummariesCall = mkOgmiosCallTypeNoArgs "queryLedgerState/eraSummaries"

-- | Queries Ogmios for the current protocol parameters
queryProtocolParametersCall :: JsonRpc2Call Unit OgmiosProtocolParameters
queryProtocolParametersCall = mkOgmiosCallTypeNoArgs
  "queryLedgerState/protocolParameters"

-- | Queries Ogmios for the chain’s current tip.
queryChainTipCall :: JsonRpc2Call Unit ChainTipQR
queryChainTipCall = mkOgmiosCallTypeNoArgs "queryNetwork/tip"

-- | Queries Ogmios for pool parameters of all pools or of the provided pools.
queryStakePoolsCall :: JsonRpc2Call StakePoolsQueryArgument PoolParametersR
queryStakePoolsCall = mkOgmiosCallType
  { method: "queryLedgerState/stakePools"
  , params: identity
  }

queryDelegationsAndRewards
  :: JsonRpc2Call (Array String) DelegationsAndRewardsR -- todo: whats string? git blame line below to restore
queryDelegationsAndRewards = mkOgmiosCallType
  { method: "queryLedgerState/rewardAccountSummaries"
  , params: \skhs ->
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
submitTxCall :: JsonRpc2Call (TxHash /\ CborBytes) SubmitTxR
submitTxCall = mkOgmiosCallType
  { method: "submitTransaction"
  , params: \(_ /\ cbor) ->
      { transaction: { cbor: cborBytesToHex cbor }
      }
  }

-- | Evaluates the execution units of scripts present in a given transaction,
-- | without actually submitting the transaction.
evaluateTxCall :: JsonRpc2Call (CborBytes /\ AdditionalUtxoSet) TxEvaluationR
evaluateTxCall = mkOgmiosCallType
  { method: "evaluateTransaction"
  , params: \(cbor /\ utxoqr) ->
      { transaction: { cbor: cborBytesToHex cbor }
      , additionalUtxo: utxoqr
      }
  }

--------------------------------------------------------------------------------
-- Local Tx Monitor Protocol
-- https://ogmios.dev/mini-protocols/local-tx-monitor/
--------------------------------------------------------------------------------

acquireMempoolSnapshotCall :: JsonRpc2Call Unit MempoolSnapshotAcquired
acquireMempoolSnapshotCall =
  mkOgmiosCallTypeNoArgs "acquireMempool"

mempoolSnapshotHasTxCall
  :: MempoolSnapshotAcquired -> JsonRpc2Call TxHash HasTxR
mempoolSnapshotHasTxCall _ = mkOgmiosCallType
  { method: "hasTransaction"
  , params: { id: _ }
  }

mempoolSnapshotNextTxCall
  :: MempoolSnapshotAcquired -> JsonRpc2Call Unit MaybeMempoolTransaction
mempoolSnapshotNextTxCall _ = mkOgmiosCallType
  { method: "nextTransaction"
  , params: const { fields: "all" }
  }

mempoolSnapshotSizeAndCapacityCall
  :: MempoolSnapshotAcquired -> JsonRpc2Call Unit MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacityCall _ =
  mkOgmiosCallTypeNoArgs "sizeOfMempool"

releaseMempoolCall
  :: MempoolSnapshotAcquired -> JsonRpc2Call Unit ReleasedMempool
releaseMempoolCall _ =
  mkOgmiosCallTypeNoArgs "releaseMempool"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

mkOgmiosCallTypeNoArgs
  :: forall (o :: Type). DecodeOgmios o => String -> JsonRpc2Call Unit o
mkOgmiosCallTypeNoArgs method =
  mkOgmiosCallType { method, params: const {} }

mkOgmiosCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonRpc2Request a)
  => DecodeOgmios o
  => { method :: String, params :: i -> a }
  -> JsonRpc2Call i o
mkOgmiosCallType =
  mkCallType { jsonrpc: "2.0" }

--------------------------------------------------------------------------------
-- Local Tx Monitor Query Response & Parsing
--------------------------------------------------------------------------------

newtype HasTxR = HasTxR Boolean

derive instance Newtype HasTxR _

instance DecodeOgmios HasTxR where
  decodeOgmios r = HasTxR <$> decodeResult r

newtype MempoolSnapshotAcquired = AwaitAcquired Slot

instance Show MempoolSnapshotAcquired where
  show (AwaitAcquired slot) = "(AwaitAcquired " <> show slot <> ")"

instance DecodeAeson MempoolSnapshotAcquired where
  decodeAeson =
    -- todo: ignoring "acquired": "mempool"
    map AwaitAcquired <<< aesonObject (flip getField "slot")

instance DecodeOgmios MempoolSnapshotAcquired where
  decodeOgmios = decodeResult

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
    capacity <- getField o "maxCapacity"
    currentSize <- getField o "currentSize"
    numberOfTxs <- (flip getField "transactions") >=> (flip getField "count") $
      o
    pure $ wrap { capacity, currentSize, numberOfTxs }

instance DecodeOgmios MempoolSizeAndCapacity where
  decodeOgmios = decodeResult

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
    raw <- o .: "cbor"
    pure $ MempoolTransaction { id, raw }

newtype MaybeMempoolTransaction = MaybeMempoolTransaction
  (Maybe MempoolTransaction)

derive instance Newtype MaybeMempoolTransaction _

instance DecodeOgmios MaybeMempoolTransaction where
  decodeOgmios r = wrap <$> decodeResult r

data ReleasedMempool = ReleasedMempool

derive instance Generic ReleasedMempool _

instance Show ReleasedMempool where
  show = genericShow

instance DecodeAeson ReleasedMempool where
  decodeAeson = aesonObject \o -> do
    released <- o .: "released"
    flip aesonString released $ \s ->
      if s == "mempool" then
        pure $ ReleasedMempool
      else
        Left (UnexpectedValue $ Argonaut.encodeString s)

instance DecodeOgmios ReleasedMempool where
  decodeOgmios = decodeResult

---------------- TX SUBMISSION QUERY RESPONSE & PARSING

submitSuccessPartialResp
  :: TxHash -> { result :: { transaction :: { id :: TxHash } } }
submitSuccessPartialResp txHash =
  { "result": { "transaction": { "id": txHash } } }

data SubmitTxR
  = SubmitTxSuccess TxHash
  | SubmitFail OgmiosError

derive instance Generic SubmitTxR _

instance Show SubmitTxR where
  show = genericShow

type TxHash = ByteArray

type OgmiosError = { code :: Int, message :: String, data :: Aeson }

instance DecodeOgmios SubmitTxR where
  decodeOgmios response =
    ( bimap DecodingError SubmitTxSuccess <<< decodeTxHash =<< decodeResult
        response
    )
      <|> (SubmitFail <$> decodeError response)

    where
    decodeTxHash :: Aeson -> Either JsonDecodeError TxHash
    decodeTxHash = aesonObject $ \o ->
      ( getField o "transaction" >>= flip getField "id" >>= hexToByteArray
          >>> maybe (Left $ TypeMismatch "Expected hexstring") pure
      )

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

instance DecodeOgmios OgmiosSystemStart where
  decodeOgmios = decodeResult

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

instance DecodeOgmios CurrentEpoch where
  decodeOgmios = decodeResult

---------------- ERA SUMMARY QUERY RESPONSE & PARSING

newtype OgmiosEraSummaries = OgmiosEraSummaries EraSummaries

derive instance Generic OgmiosEraSummaries _
derive instance Newtype OgmiosEraSummaries _
derive newtype instance Eq OgmiosEraSummaries

instance Show OgmiosEraSummaries where
  show = genericShow

instance DecodeAeson OgmiosEraSummaries where
  -- There is some differences between ogmios 6.0 spec and actual results
  -- in "start" "end" fields and "slotLength".
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
      slotLength <- wrap <$>
        ( (*) slotLengthFactor <$>
            (flip getField "seconds" =<< getField o "slotLength")
        )
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
        , "slotLength": { "seconds": params.slotLength }
        , "safeZone": params.safeZone
        }

-- Ogmios returns `slotLength` in seconds, and we use milliseconds,
-- so we need to convert between them.
slotLengthFactor :: Number
slotLengthFactor = 1000.0

instance DecodeOgmios OgmiosEraSummaries where
  decodeOgmios = decodeResult

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

instance DecodeOgmios DelegationsAndRewardsR where
  decodeOgmios = decodeResult

---------------- POOL PARAMETERS REQUEST & PARSING

-- Nothing queries all pools, otherwise query selected pools.
newtype StakePoolsQueryArgument = StakePoolsQueryArgument
  (Maybe (Array PoolPubKeyHash))

derive instance Newtype StakePoolsQueryArgument _

instance EncodeAeson StakePoolsQueryArgument where
  encodeAeson a = do
    maybe
      (encodeAeson {})
      ( \poolPkhs -> encodeAeson
          { stakePools: map (\pool -> { id: pool }) poolPkhs }
      )
      (unwrap a)

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

newtype PoolParametersR = PoolParametersR (Map PoolPubKeyHash PoolParameters)

derive instance Newtype PoolParametersR _
derive instance Generic PoolParametersR _

instance Show PoolParametersR where
  show = genericShow

instance DecodeAeson PoolParametersR where
  decodeAeson aeson = do
    obj :: Object (Object Aeson) <- decodeAeson aeson
    kvs <- for (Object.toUnfoldable obj :: Array _) \(Tuple k objParams) -> do
      poolPkh <- decodeAeson $ fromString k
      poolParams <- decodePoolParameters objParams
      pure $ poolPkh /\ poolParams
    pure $ PoolParametersR $ Map.fromFoldable kvs

instance DecodeOgmios PoolParametersR where
  decodeOgmios = decodeResult

decodePoolParameters :: Object Aeson -> Either JsonDecodeError PoolParameters
decodePoolParameters objParams = do
  vrfKeyhash <- decodeVRFKeyHash =<< objParams .: "vrfVerificationKeyHash"
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

showRedeemerPointer :: RedeemerPointer -> String
showRedeemerPointer ptr = show ptr.redeemerTag <> ":" <> show ptr.redeemerIndex

type ExecutionUnits = { memory :: Natural, steps :: Natural }

newtype TxEvaluationR = TxEvaluationR
  (Either TxEvaluationFailure TxEvaluationResult)

derive instance Newtype TxEvaluationR _
derive instance Generic TxEvaluationR _

instance Show TxEvaluationR where
  show = genericShow

instance DecodeOgmios TxEvaluationR where
  decodeOgmios r = wrap <$>
    ( (Right <$> decodeResult r)
        <|> (Left <$> decodeError r)
    )

-- (anyResponse (map (TxEvaluationR <<< Left) <<< decodeAeson) (map (TxEvaluationR <<< Right) <<< decodeAeson))

newtype TxEvaluationResult = TxEvaluationResult
  (Map RedeemerPointer ExecutionUnits)

derive instance Newtype TxEvaluationResult _
derive instance Generic TxEvaluationResult _

instance Show TxEvaluationResult where
  show = genericShow

instance DecodeAeson TxEvaluationResult where
  decodeAeson = aesonArray $ \array -> do
    TxEvaluationResult <<< Map.fromFoldable <$>
      traverse decodeRdmrPtrExUnitsItem array

    where
    decodeRdmrPtrExUnitsItem
      :: Aeson -> Either JsonDecodeError (RedeemerPointer /\ ExecutionUnits)
    decodeRdmrPtrExUnitsItem elem = do
      res
        :: { validator :: String
           , budget :: { memory :: Natural, cpu :: Natural }
           } <- decodeAeson elem
      redeemerPtr <- decodeRedeemerPointer res.validator
      pure $ redeemerPtr /\ { memory: res.budget.memory, steps: res.budget.cpu }

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
  | MissingRequiredDatums (Array OgmiosDatum)
  | MissingRequiredScripts (Array RedeemerPointer)
  | ValidatorFailed { error :: String, traces :: Array String }
  | UnknownInputReferencedByRedeemer OgmiosTxIn
  | NonScriptInputReferencedByRedeemer OgmiosTxIn
  | NoCostModelForLanguage (Array String)
  | InternalLedgerTypeConversionError String

derive instance Generic ScriptFailure _

instance Show ScriptFailure where
  show = genericShow

-- The following cases are fine to fall through into unparsed error:
-- IncompatibleEra
-- AdditionalUtxoOverlap
-- NotEnoughSynced
-- CannotCreateEvaluationContext
data TxEvaluationFailure
  = UnparsedError OgmiosError
  | ScriptFailures (Map RedeemerPointer (Array ScriptFailure))

derive instance Generic TxEvaluationFailure _

instance Show TxEvaluationFailure where
  show = genericShow

instance DecodeAeson ScriptFailure where
  decodeAeson aeson = do
    error :: OgmiosError <- decodeAeson aeson
    case error.code of
      3011 -> do
        res :: { missingScript :: Array String } <- decodeAeson error.data
        MissingRequiredScripts <$> traverse decodeRedeemerPointer
          res.missingScript
      3012 -> do
        res :: { validationError :: String, traces :: Array String } <-
          decodeAeson error.data
        pure $ ValidatorFailed
          { error: res.validationError, traces: res.traces }
      3013 -> do
        res
          :: { unsuitableOutputReference ::
                 { transaction :: { id :: String }, index :: Int }
             } <- decodeAeson error.data
        pure $ NonScriptInputReferencedByRedeemer
          { index: res.unsuitableOutputReference.index
          , txId: res.unsuitableOutputReference.transaction.id
          }
      3110 -> do
        res :: { extraneousRedeemers :: Array String } <- decodeAeson error.data
        ExtraRedeemers <$> traverse decodeRedeemerPointer
          res.extraneousRedeemers
      3111 -> do
        res :: { missingDatums :: Array String } <- decodeAeson error.data
        pure $ MissingRequiredDatums res.missingDatums
      3117 -> do
        res
          :: { unknownOutputReference ::
                 { transaction :: { id :: String }, index :: Int }
             } <- decodeAeson error.data
        let res' = res.unknownOutputReference
        pure $ UnknownInputReferencedByRedeemer
          { index: res'.index, txId: res'.transaction.id }
      3115 -> do
        res :: { missingCostModels :: Array String } <- decodeAeson error.data
        pure $ NoCostModelForLanguage res.missingCostModels
      3999 -> pure $ InternalLedgerTypeConversionError error.message
      _ -> Left $ TypeMismatch $ "Unknown ogmios error code: " <> show
        error.code

instance DecodeAeson TxEvaluationFailure where
  decodeAeson aeson = do
    error :: OgmiosError <- decodeAeson aeson
    case error.code of
      -- ScriptExecutionFailure
      3010 -> flip aesonArray error.data $
        ( \array ->
            ( ScriptFailures <<< map Array.fromFoldable <<< collectIntoMap <$>
                traverse parseElem array
            )
        )
      -- All other errors
      _ -> pure $ UnparsedError error

    where
    parseElem elem = do
      res :: { validator :: String, error :: ScriptFailure } <- decodeAeson elem
      (_ /\ res.error) <$> decodeRedeemerPointer res.validator

    collectIntoMap :: forall k v. Ord k => Array (k /\ v) -> Map k (List v)
    collectIntoMap = foldl
      ( \m (k /\ v) -> Map.alter
          (maybe (Just $ List.singleton v) (Just <<< List.Cons v))
          k
          m
      )
      Map.empty

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
  , "minFeeConstant" ::
      { "lovelace" :: UInt }
  , "minUtxoDepositCoefficient" :: BigInt
  , "maxBlockBodySize" ::
      { "bytes" :: UInt }
  , "maxBlockHeaderSize" ::
      { "bytes" :: UInt }
  , "maxTransactionSize" ::
      { "bytes" :: UInt }
  , "maxValueSize" ::
      { "bytes" :: UInt }
  , "stakeCredentialDeposit" ::
      { "lovelace" :: BigInt }
  , "stakePoolDeposit" ::
      { "lovelace" :: BigInt }
  , "stakePoolRetirementEpochBound" :: BigInt
  , "desiredNumberOfStakePools" :: UInt
  , "stakePoolPledgeInfluence" :: PParamRational
  , "monetaryExpansion" :: PParamRational
  , "treasuryExpansion" :: PParamRational
  , "version" ::
      { "major" :: UInt
      , "minor" :: UInt
      }
  , "minStakePoolCost" ::
      { "lovelace" :: BigInt }
  , "plutusCostModels" ::
      { "plutus:v1" :: Array Csl.Int
      , "plutus:v2" :: Maybe (Array Csl.Int)
      }
  , "scriptExecutionPrices" ::
      { "memory" :: PParamRational
      , "cpu" :: PParamRational
      }
  , "maxExecutionUnitsPerTransaction" ::
      { "memory" :: BigInt
      , "cpu" :: BigInt
      }
  , "maxExecutionUnitsPerBlock" ::
      { "memory" :: BigInt
      , "cpu" :: BigInt
      }
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
    pure $ OgmiosProtocolParameters $ ProtocolParameters
      { protocolVersion: ps.version.major /\ ps.version.minor
      -- The following two parameters were removed from Babbage
      , decentralization: zero
      , extraPraosEntropy: Nothing
      , maxBlockHeaderSize: ps.maxBlockHeaderSize.bytes
      , maxBlockBodySize: ps.maxBlockBodySize.bytes
      , maxTxSize: ps.maxTransactionSize.bytes
      , txFeeFixed: ps.minFeeConstant.lovelace
      , txFeePerByte: ps.minFeeCoefficient
      , stakeAddressDeposit: Coin ps.stakeCredentialDeposit.lovelace
      , stakePoolDeposit: Coin ps.stakePoolDeposit.lovelace
      , minPoolCost: Coin ps.minStakePoolCost.lovelace
      , poolRetireMaxEpoch: Epoch ps.stakePoolRetirementEpochBound
      , stakePoolTargetNum: ps.desiredNumberOfStakePools
      , poolPledgeInfluence: unwrap ps.stakePoolPledgeInfluence
      , monetaryExpansion: unwrap ps.monetaryExpansion
      , treasuryCut: unwrap ps.treasuryExpansion -- Rational
      , coinsPerUtxoUnit: CoinsPerUtxoByte (Coin ps.minUtxoDepositCoefficient)
      , costModels: Costmdls $ Map.fromFoldable $ catMaybes
          [ pure
              ( PlutusV1 /\ CostModel
                  ps.plutusCostModels."plutus:v1"
              )
          , (PlutusV2 /\ _) <<< CostModel <$>
              ps.plutusCostModels."plutus:v2"
          ]
      , prices: prices
      , maxTxExUnits: decodeExUnits ps.maxExecutionUnitsPerTransaction
      , maxBlockExUnits: decodeExUnits ps.maxExecutionUnitsPerBlock
      , maxValueSize: ps.maxValueSize.bytes
      , collateralPercent: ps.collateralPercentage
      , maxCollateralInputs: ps.maxCollateralInputs
      }
    where
    decodeExUnits
      :: { memory :: BigInt, cpu :: BigInt } -> ExUnits
    decodeExUnits { memory, cpu } = { mem: memory, steps: cpu }

    decodePrices
      :: ProtocolParametersRaw -> Either JsonDecodeError ExUnitPrices
    decodePrices ps = note (TypeMismatch "ExUnitPrices") do
      memPrice <- rationalToSubcoin ps.scriptExecutionPrices.memory
      stepPrice <- rationalToSubcoin ps.scriptExecutionPrices.cpu
      pure { memPrice, stepPrice } -- ExUnits

instance DecodeOgmios OgmiosProtocolParameters where
  decodeOgmios = decodeResult

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

instance DecodeOgmios ChainTipQR where
  decodeOgmios = decodeResult

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
  , id :: OgmiosBlockHeaderHash
  }

---------------- ADDITIONAL UTXO MAP REQUEST

newtype AdditionalUtxoSet = AdditionalUtxoSet OgmiosUtxoMap

derive instance Newtype AdditionalUtxoSet _

derive newtype instance Show AdditionalUtxoSet

-- Ogmios tx input
type OgmiosTxOutRef =
  { txId :: String
  , index :: UInt.UInt
  }

type OgmiosTxOut =
  { address :: OgmiosAddress
  , value :: Value
  , datumHash :: Maybe String
  , datum :: Maybe String
  , script :: Maybe ScriptRef
  }

type OgmiosUtxoMap = Map OgmiosTxOutRef OgmiosTxOut

instance EncodeAeson AdditionalUtxoSet where
  encodeAeson (AdditionalUtxoSet m) =
    encodeAeson $ encode <$> utxos

    where
    utxos :: Array (OgmiosTxOutRef /\ OgmiosTxOut)
    utxos = Map.toUnfoldable m

    encode :: (OgmiosTxOutRef /\ OgmiosTxOut) -> Aeson
    encode (inp /\ out) = encodeAeson $
      { "transaction": { "id": inp.txId }
      , "index": inp.index
      , "address": out.address
      , "datumHash": out.datumHash
      , "datum": out.datum
      , "script": encodeScriptRef <$> out.script
      , "value": encodeValue out.value
      }

    encodeNativeScript :: NativeScript -> Aeson
    encodeNativeScript (ScriptPubkey s) =
      encodeAeson { "clause": "signature", "from": encodeAeson s }
    encodeNativeScript (ScriptAll ss) =
      encodeAeson { "clause": "all", "from": encodeNativeScript <$> ss }
    encodeNativeScript (ScriptAny ss) =
      encodeAeson { "clause": "any", "from": encodeNativeScript <$> ss }
    encodeNativeScript (ScriptNOfK n ss) =
      encodeAeson
        { "clause": "some"
        , "atLeast": BigInt.fromInt n
        , "from": encodeNativeScript <$> ss
        }
    encodeNativeScript (TimelockStart (Slot n)) =
      encodeAeson { "clause": "after", "slot": n }
    encodeNativeScript (TimelockExpiry (Slot n)) =
      encodeAeson { "clause": "before", "slot": n }

    encodeScriptRef :: ScriptRef -> Aeson
    encodeScriptRef (NativeScriptRef s) =
      encodeAeson $
        { "language": "native", "cbor": s, "json": (encodeNativeScript s) }
    encodeScriptRef (PlutusScriptRef (PlutusScript (s /\ PlutusV1))) =
      encodeAeson { "language": "plutus:v1", "cbor": byteArrayToHex s }
    encodeScriptRef (PlutusScriptRef (PlutusScript (s /\ PlutusV2))) =
      encodeAeson { "language": "plutus:v2", "cbor": byteArrayToHex s }

    encodeValue :: Value -> Aeson
    encodeValue value = encodeMap $ Map.union adaPart nonAdaPart
      where
      adaPart = Map.fromFoldable
        [ ( "ada" /\
              ( Map.fromFoldable
                  [ ("lovelace" /\ (value # valueToCoin # getLovelace)) ]
              )
          )
        ]
      nonAdaPart = mapKeys (byteArrayToHex <<< getCurrencySymbol)
        $ map (mapKeys (byteArrayToHex <<< getTokenName))
        $ unwrapNonAdaAsset
        $ getNonAdaAsset value

      mapKeys :: forall k1 k2 a. Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
      mapKeys f = (Map.toUnfoldable :: Map k1 a -> Array (k1 /\ a)) >>> foldl
        (\m' (k /\ v) -> Map.insert (f k) v m')
        Map.empty

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
