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
      , IllFormedExecutionBudget
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
  , StakePoolsQueryArgument(StakePoolsQueryArgument)
  , TxEvaluationFailure(UnparsedError, AdditionalUtxoOverlap, ScriptFailures)
  , TxEvaluationResult(TxEvaluationResult)
  , TxEvaluationR(TxEvaluationR)
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
  , parseIpv6String
  , rationalToSubcoin
  , showRedeemerPointer
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(AtKey, TypeMismatch, UnexpectedValue, MissingValue)
  , caseAesonArray
  , caseAesonNull
  , caseAesonObject
  , caseAesonString
  , decodeAeson
  , encodeAeson
  , fromArray
  , fromString
  , getField
  , isNull
  , stringifyAeson
  , (.:)
  , (.:?)
  )
import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Serialization.Lib (fromBytes, ipv4_new)
import Cardano.Types
  ( Bech32String
  , BigNum(BigNum)
  , Language(PlutusV1, PlutusV2, PlutusV3)
  , RedeemerTag
  , VRFKeyHash(VRFKeyHash)
  )
import Cardano.Types.AssetName (unAssetName)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum (fromBigInt, fromString) as BigNum
import Cardano.Types.CborBytes (CborBytes)
import Cardano.Types.Coin (Coin(Coin))
import Cardano.Types.CostModel (CostModel(CostModel))
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.ExUnitPrices (ExUnitPrices(ExUnitPrices))
import Cardano.Types.ExUnits (ExUnits(ExUnits))
import Cardano.Types.Int as Cardano
import Cardano.Types.Ipv4 (Ipv4(Ipv4))
import Cardano.Types.Ipv6 (Ipv6)
import Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  )
import Cardano.Types.PlutusScript (PlutusScript(PlutusScript))
import Cardano.Types.PoolMetadata (PoolMetadata(PoolMetadata))
import Cardano.Types.PoolPubKeyHash (PoolPubKeyHash)
import Cardano.Types.RedeemerTag
  ( RedeemerTag(Spend, Mint, Cert, Reward, Vote, Propose)
  ) as RedeemerTag
import Cardano.Types.Relay
  ( Relay(SingleHostAddr, SingleHostName, MultiHostName)
  )
import Cardano.Types.RewardAddress (RewardAddress)
import Cardano.Types.RewardAddress as RewardAddress
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.ScriptRef (ScriptRef(NativeScriptRef, PlutusScriptRef))
import Cardano.Types.Slot (Slot(Slot))
import Cardano.Types.TransactionHash (TransactionHash)
import Cardano.Types.URL (URL(URL))
import Cardano.Types.UnitInterval (UnitInterval(UnitInterval))
import Cardano.Types.Value (Value, getMultiAsset, valueToCoin)
import Control.Alt ((<|>))
import Control.Alternative (guard)
import Ctl.Internal.Helpers (encodeMap, showWithParens)
import Ctl.Internal.QueryM.JsonRpc2
  ( class DecodeOgmios
  , JsonRpc2Call
  , JsonRpc2Request
  , OgmiosError
  , decodeErrorOrResult
  , decodeResult
  , mkCallType
  )
import Ctl.Internal.Types.EraSummaries
  ( EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  , EraSummaryTime(EraSummaryTime)
  )
import Ctl.Internal.Types.ProtocolParameters
  ( ProtocolParameters(ProtocolParameters)
  )
import Ctl.Internal.Types.Rational (Rational, (%))
import Ctl.Internal.Types.Rational as Rational
import Ctl.Internal.Types.SystemStart
  ( SystemStart
  , sysStartFromOgmiosTimestamp
  , sysStartToOgmiosTimestamp
  )
import Data.Argonaut.Encode.Encoders as Argonaut
import Data.Array (catMaybes)
import Data.Array (fromFoldable, length, replicate) as Array
import Data.Bifunctor (lmap)
import Data.ByteArray (byteArrayFromIntArray, byteArrayToHex, hexToByteArray)
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
import Data.String (Pattern(Pattern), Replacement(Replacement))
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
import JS.BigInt as BigInt
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

type OgmiosAddress = Bech32String

--------------------------------------------------------------------------------
-- Local Tx Submission Protocol
-- https://ogmios.dev/mini-protocols/local-tx-submission/
--------------------------------------------------------------------------------

-- | Sends a serialized signed transaction with its full witness through the
-- | Cardano network via Ogmios.
submitTxCall :: JsonRpc2Call (TransactionHash /\ CborBytes) SubmitTxR
submitTxCall = mkOgmiosCallType
  { method: "submitTransaction"
  , params: \(_ /\ cbor) ->
      { transaction: { cbor: byteArrayToHex $ unwrap cbor }
      }
  }

-- | Evaluates the execution units of scripts present in a given transaction,
-- | without actually submitting the transaction.
evaluateTxCall :: JsonRpc2Call (CborBytes /\ AdditionalUtxoSet) TxEvaluationR
evaluateTxCall = mkOgmiosCallType
  { method: "evaluateTransaction"
  , params: \(cbor /\ utxoqr) ->
      { transaction: { cbor: byteArrayToHex $ unwrap cbor }
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
  :: MempoolSnapshotAcquired -> JsonRpc2Call TransactionHash HasTxR
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
  decodeOgmios = decodeResult (map HasTxR <<< decodeAeson)

newtype MempoolSnapshotAcquired = AwaitAcquired Slot

instance Show MempoolSnapshotAcquired where
  show (AwaitAcquired slot) = "(AwaitAcquired " <> show slot <> ")"

instance DecodeAeson MempoolSnapshotAcquired where
  decodeAeson =
    -- todo: ignoring "acquired": "mempool"
    map AwaitAcquired <<< aesonObject (flip getField "slot")

instance DecodeOgmios MempoolSnapshotAcquired where
  decodeOgmios = decodeResult decodeAeson

-- | The acquired snapshot’s size (in bytes), number of transactions, and capacity
-- | (in bytes).
newtype MempoolSizeAndCapacity = MempoolSizeAndCapacity
  { capacity :: Prim.Int
  , currentSize :: Prim.Int
  , numberOfTxs :: Prim.Int
  }

derive instance Generic MempoolSizeAndCapacity _
derive instance Newtype MempoolSizeAndCapacity _

instance Show MempoolSizeAndCapacity where
  show = genericShow

instance DecodeAeson MempoolSizeAndCapacity where
  decodeAeson = aesonObject \o -> do
    capacity <- getField o "maxCapacity" >>= flip getField "bytes"
    currentSize <- getField o "currentSize" >>= flip getField "bytes"
    numberOfTxs <- getField o "transactions" >>= flip getField "count"
    pure $ wrap { capacity, currentSize, numberOfTxs }

instance DecodeOgmios MempoolSizeAndCapacity where
  decodeOgmios = decodeResult decodeAeson

newtype MempoolTransaction = MempoolTransaction
  { id :: OgmiosTxId
  , raw :: String -- hex encoded transaction cbor
  }

derive instance Generic MempoolTransaction _
derive instance Newtype MempoolTransaction _

newtype MaybeMempoolTransaction = MaybeMempoolTransaction
  (Maybe MempoolTransaction)

instance DecodeAeson MaybeMempoolTransaction where
  decodeAeson aeson = do
    { transaction: tx } :: { transaction :: Aeson } <- decodeAeson aeson
    res <-
      ( do
          tx' :: { id :: String, cbor :: String } <- decodeAeson tx
          pure $ Just $ MempoolTransaction { id: tx'.id, raw: tx'.cbor }
      ) <|>
        ( do
            aesonNull tx
            pure Nothing
        )
    pure $ MaybeMempoolTransaction $ res

derive instance Newtype MaybeMempoolTransaction _

instance DecodeOgmios MaybeMempoolTransaction where
  decodeOgmios = decodeResult decodeAeson

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
  decodeOgmios = decodeResult decodeAeson

---------------- TX SUBMISSION QUERY RESPONSE & PARSING

submitSuccessPartialResp
  :: TransactionHash
  -> { result :: { transaction :: { id :: TransactionHash } } }
submitSuccessPartialResp txHash =
  { "result": { "transaction": { "id": txHash } } }

data SubmitTxR
  = SubmitTxSuccess TransactionHash
  | SubmitFail OgmiosError

derive instance Generic SubmitTxR _

instance Show SubmitTxR where
  show = genericShow

instance DecodeOgmios SubmitTxR where
  decodeOgmios = decodeErrorOrResult
    { parseError: decodeError }
    { parseResult: map SubmitTxSuccess <<< decodeTxHash }

    where

    decodeError aeson = map SubmitFail do
      -- With Ogmios 5.6 we failed with error on deserialization error, so we do now as well
      err :: OgmiosError <- decodeAeson aeson
      let code = (unwrap err).code
      -- as of 7.11.23 it's in {3005} u [3100, 3159] range
      if (3000 <= code && code <= 3999) then
        pure err
      else
        Left $ TypeMismatch
          "Expected error code in a range [3000, 3999]"

    decodeTxHash :: Aeson -> Either JsonDecodeError TransactionHash
    decodeTxHash = aesonObject \o -> do
      txHashHex <- getField o "transaction" >>= flip getField "id"
      note (TypeMismatch "Expected hexstring of TransactionHash") $
        hexToByteArray txHashHex >>= fromBytes >>> map wrap

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
  decodeOgmios = decodeResult decodeAeson

---------------- CURRENT EPOCH QUERY RESPONSE & PARSING
newtype CurrentEpoch = CurrentEpoch BigNum

derive instance Generic CurrentEpoch _
derive instance Newtype CurrentEpoch _
derive newtype instance DecodeAeson CurrentEpoch
derive newtype instance EncodeAeson CurrentEpoch
derive newtype instance Eq CurrentEpoch
derive newtype instance Ord CurrentEpoch

instance Show CurrentEpoch where
  show (CurrentEpoch ce) = showWithParens "CurrentEpoch" ce

instance DecodeOgmios CurrentEpoch where
  decodeOgmios = decodeResult decodeAeson

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
    decodeEraSummaryTime :: Aeson -> Either JsonDecodeError EraSummaryTime
    decodeEraSummaryTime = aesonObject \obj -> do
      time <- flip getField "seconds" =<< getField obj "time"
      slot <- getField obj "slot"
      epoch <- getField obj "epoch"
      pure $ wrap { time, slot, epoch }

    decodeEraSummary :: Aeson -> Either JsonDecodeError EraSummary
    decodeEraSummary = aesonObject \o -> do
      start <- decodeEraSummaryTime =<< getField o "start"
      -- The field "end" is required by Ogmios API, but it can optionally return
      -- Null, so we want to fail if the field is absent but make Null value
      -- acceptable in presence of the field (hence why "end" is wrapped in
      -- `Maybe`).
      end' <- getField o "end"
      end <-
        if isNull end' then pure Nothing else Just <$> decodeEraSummaryTime end'
      parameters <- decodeEraSummaryParameters =<< getField o "parameters"
      pure $ wrap { start, end, parameters }

    decodeEraSummaryParameters
      :: Object Aeson -> Either JsonDecodeError EraSummaryParameters
    decodeEraSummaryParameters o = do
      epochLength <- getField o "epochLength"
      slotLength <- flip getField "milliseconds" =<< getField o "slotLength"
      safeZone <- fromMaybe zero <$> getField o "safeZone"
      pure $ wrap { epochLength, slotLength, safeZone }

instance EncodeAeson OgmiosEraSummaries where
  encodeAeson (OgmiosEraSummaries (EraSummaries eraSummaries)) =
    fromArray $ map encodeEraSummary eraSummaries
    where
    encodeEraSummaryTime :: EraSummaryTime -> Aeson
    encodeEraSummaryTime (EraSummaryTime { time, slot, epoch }) =
      encodeAeson { "time": { "seconds": time }, "slot": slot, "epoch": epoch }

    encodeEraSummary :: EraSummary -> Aeson
    encodeEraSummary (EraSummary { start, end, parameters }) =
      encodeAeson
        { "start": encodeEraSummaryTime start
        , "end": encodeEraSummaryTime <$> end
        , "parameters": encodeEraSummaryParameters parameters
        }

    encodeEraSummaryParameters :: EraSummaryParameters -> Aeson
    encodeEraSummaryParameters (EraSummaryParameters params) =
      encodeAeson
        { "epochLength": params.epochLength
        , "slotLength": { "milliseconds": params.slotLength }
        , "safeZone": params.safeZone
        }

instance DecodeOgmios OgmiosEraSummaries where
  decodeOgmios = decodeResult decodeAeson

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
  decodeOgmios = decodeResult decodeAeson

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
  decodeOgmios = decodeResult decodeAeson

decodePoolParameters :: Object Aeson -> Either JsonDecodeError PoolParameters
decodePoolParameters objParams = do
  vrfKeyhash <- decodeVRFKeyHash =<< objParams .: "vrfVerificationKeyHash"
  pledge <- objParams .: "pledge" >>= aesonObject \obj ->
    obj .: "ada" >>= flip getField "lovelace"
  cost <- objParams .: "cost" >>= aesonObject \obj ->
    obj .: "ada" >>= flip getField "lovelace"
  margin <- decodeUnitInterval =<< objParams .: "margin"
  rewardAccount <- objParams .: "rewardAccount" >>=
    RewardAddress.fromBech32 >>> note (TypeMismatch "RewardAddress")
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
  note (TypeMismatch "VRFKeyHash") $ VRFKeyHash <$> fromBytes vrfKeyhashBytes

decodeUnitInterval :: Aeson -> Either JsonDecodeError UnitInterval
decodeUnitInterval aeson = do
  str <- decodeAeson aeson
  case String.split (Pattern "/") str of
    [ num, den ] -> do
      numerator <- note (TypeMismatch "BigNum") $ BigNum.fromString num
      denominator <- note (TypeMismatch "BigNum") $ BigNum.fromString den
      pure $ UnitInterval
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
      Ipv4 <<< ipv4_new <$> note (TypeMismatch "Ipv4")
        (byteArrayFromIntArray ints)
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
  decodeCbor <<< wrap =<< hexToByteArray padded

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
  hash <- obj .: "hash" >>=
    (hexToByteArray >>> map wrap >=> decodeCbor) >>>
      note (TypeMismatch "PoolMetadataHash")
  url <- obj .: "url" <#> URL
  pure $ PoolMetadata { hash, url }

---------------- TX EVALUATION QUERY RESPONSE & PARSING

type RedeemerPointer = { redeemerTag :: RedeemerTag, redeemerIndex :: UInt }

showRedeemerPointer :: RedeemerPointer -> String
showRedeemerPointer ptr = show ptr.redeemerTag <> ":" <> show ptr.redeemerIndex

type ExecutionUnits = { memory :: BigNum, steps :: BigNum }

type OgmiosRedeemerPtr = { index :: UInt, purpose :: String }

newtype TxEvaluationR = TxEvaluationR
  (Either TxEvaluationFailure TxEvaluationResult)

derive instance Newtype TxEvaluationR _
derive instance Generic TxEvaluationR _

instance Show TxEvaluationR where
  show = genericShow

instance DecodeOgmios TxEvaluationR where
  decodeOgmios = decodeErrorOrResult
    { parseError: map (wrap <<< Left) <<< decodeAeson }
    { parseResult: map (wrap <<< Right) <<< decodeAeson }

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
        :: { validator :: OgmiosRedeemerPtr
           , budget :: { memory :: BigNum, cpu :: BigNum }
           } <- decodeAeson elem
      redeemerPtr <- decodeRedeemerPointer res.validator
      pure $ redeemerPtr /\ { memory: res.budget.memory, steps: res.budget.cpu }

redeemerTypeMismatch :: JsonDecodeError
redeemerTypeMismatch = TypeMismatch
  "Expected redeemer to be one of: \
  \(spend|mint|publish|withdraw|vote|propose)"

decodeRedeemerPointer
  :: { index :: UInt, purpose :: String }
  -> Either JsonDecodeError RedeemerPointer
decodeRedeemerPointer { index: redeemerIndex, purpose } =
  note redeemerTypeMismatch $ { redeemerTag: _, redeemerIndex } <$>
    redeemerTagFromString purpose

redeemerTagFromString :: String -> Maybe RedeemerTag
redeemerTagFromString = case _ of
  "spend" -> Just RedeemerTag.Spend
  "mint" -> Just RedeemerTag.Mint
  "publish" -> Just RedeemerTag.Cert
  "withdraw" -> Just RedeemerTag.Reward
  "vote" -> Just RedeemerTag.Vote
  "propose" -> Just RedeemerTag.Propose
  _ -> Nothing

type OgmiosDatum = String
type OgmiosScript = String
type OgmiosTxId = String
type OgmiosTxIn = { txId :: OgmiosTxId, index :: Prim.Int }

-- | Reason a script failed.
--
-- The type definition is a least common denominator between Ogmios v6 format used by ogmios backend
-- and ogmios v5.6 format used by blockfrost backend
data ScriptFailure
  = ExtraRedeemers (Array RedeemerPointer)
  | MissingRequiredDatums
      { missing :: (Array OgmiosDatum)
      , provided :: Maybe (Array OgmiosDatum)
      }
  | MissingRequiredScripts
      { missing :: Array RedeemerPointer
      , resolved :: Maybe (Map RedeemerPointer ScriptHash)
      }
  | ValidatorFailed { error :: String, traces :: Array String }
  | UnknownInputReferencedByRedeemer (Array OgmiosTxIn)
  | NonScriptInputReferencedByRedeemer OgmiosTxIn
  | NoCostModelForLanguage (Array String)
  | InternalLedgerTypeConversionError String
  | IllFormedExecutionBudget (Maybe ExecutionUnits)

derive instance Generic ScriptFailure _

instance Show ScriptFailure where
  show = genericShow

-- The following cases are fine to fall through into unparsed error:
-- IncompatibleEra
-- NotEnoughSynced
-- CannotCreateEvaluationContext
data TxEvaluationFailure
  = UnparsedError String
  | AdditionalUtxoOverlap (Array OgmiosTxOutRef)
  | ScriptFailures (Map RedeemerPointer (Array ScriptFailure))

derive instance Generic TxEvaluationFailure _

instance Show TxEvaluationFailure where
  show = genericShow

instance DecodeAeson ScriptFailure where
  decodeAeson aeson = do
    err :: OgmiosError <- decodeAeson aeson
    let error = unwrap err
    errorData <- maybe (Left (AtKey "data" MissingValue)) pure error.data
    case error.code of
      3011 -> do
        res :: { missingScripts :: Array OgmiosRedeemerPtr } <- decodeAeson
          errorData
        missing <- traverse decodeRedeemerPointer res.missingScripts
        pure $ MissingRequiredScripts { missing: missing, resolved: Nothing }
      3012 -> do
        res :: { validationError :: String, traces :: Array String } <-
          decodeAeson errorData
        pure $ ValidatorFailed
          { error: res.validationError, traces: res.traces }
      3013 -> do
        res
          :: { unsuitableOutputReference ::
                 { transaction :: { id :: String }, index :: Prim.Int }
             } <- decodeAeson errorData
        pure $ NonScriptInputReferencedByRedeemer
          { index: res.unsuitableOutputReference.index
          , txId: res.unsuitableOutputReference.transaction.id
          }
      3110 -> do
        res :: { extraneousRedeemers :: Array OgmiosRedeemerPtr } <- decodeAeson
          errorData
        ExtraRedeemers <$> traverse decodeRedeemerPointer
          res.extraneousRedeemers
      3111 -> do
        res :: { missingDatums :: Array String } <- decodeAeson errorData
        pure $ MissingRequiredDatums
          { missing: res.missingDatums, provided: Nothing }
      3117 -> do
        res
          :: { unknownOutputReferences ::
                 Array { transaction :: { id :: String }, index :: Prim.Int }
             } <- decodeAeson errorData
        pure $ UnknownInputReferencedByRedeemer $
          map (\x -> { index: x.index, txId: x.transaction.id })
            res.unknownOutputReferences
      3115 -> do
        res :: { missingCostModels :: Array String } <- decodeAeson errorData
        pure $ NoCostModelForLanguage res.missingCostModels
      -- this would actually fail at decoding error.data but it's good
      3999 -> pure $ InternalLedgerTypeConversionError error.message
      _ -> Left $ TypeMismatch $ "Unknown ogmios error code: " <> show
        error.code

instance DecodeAeson TxEvaluationFailure where
  decodeAeson aeson = do
    error :: OgmiosError <- decodeAeson aeson
    let code = (unwrap error).code
    errorData <- maybe (Left (AtKey "data" MissingValue)) pure
      (unwrap error).data
    case code of
      -- ScriptExecutionFailure
      3010 -> flip aesonArray errorData $
        ( \array ->
            ( ScriptFailures <<< map Array.fromFoldable <<< collectIntoMap <$>
                traverse parseElem array
            )
        )
      -- Overlapping AdditionalUtxo
      3002 -> do
        res
          :: { overlappingOutputReferences ::
                 Array { transaction :: { id :: String }, index :: UInt }
             } <- decodeAeson errorData
        pure $ AdditionalUtxoOverlap $ map
          (\elem -> { txId: elem.transaction.id, index: elem.index })
          res.overlappingOutputReferences
      -- All other errors
      _ -> pure $ UnparsedError $ stringifyAeson aeson

    where
    parseElem elem = do
      res :: { validator :: OgmiosRedeemerPtr, error :: ScriptFailure } <-
        decodeAeson elem
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

rationalToSubcoin :: PParamRational -> Maybe UnitInterval
rationalToSubcoin (PParamRational rat) = do
  numerator <- BigNum.fromBigInt $ Rational.numerator rat
  denominator <- BigNum.fromBigInt $ Rational.denominator rat
  pure $ UnitInterval { numerator, denominator }

type OgmiosAdaLovelace = { "ada" :: { "lovelace" :: BigNum } }
type OgmiosBytes = { "bytes" :: UInt }

-- | A type that corresponds to Ogmios response.
type ProtocolParametersRaw =
  { "minFeeCoefficient" :: UInt
  , "minFeeConstant" :: OgmiosAdaLovelace
  , "minUtxoDepositCoefficient" :: BigNum
  , "maxBlockBodySize" :: OgmiosBytes
  , "maxBlockHeaderSize" :: OgmiosBytes
  , "maxTransactionSize" :: OgmiosBytes
  , "maxValueSize" :: OgmiosBytes
  , "stakeCredentialDeposit" :: OgmiosAdaLovelace
  , "stakePoolDeposit" :: OgmiosAdaLovelace
  , "stakePoolRetirementEpochBound" :: UInt
  , "desiredNumberOfStakePools" :: UInt
  , "stakePoolPledgeInfluence" :: PParamRational
  , "monetaryExpansion" :: PParamRational
  , "treasuryExpansion" :: PParamRational
  , "version" ::
      { "major" :: UInt
      , "minor" :: UInt
      }
  , "minStakePoolCost" :: OgmiosAdaLovelace
  , "plutusCostModels" ::
      { "plutus:v1" :: Array Cardano.Int
      , "plutus:v2" :: Maybe (Array Cardano.Int)
      , "plutus:v3" :: Maybe (Array Cardano.Int)
      }
  , "scriptExecutionPrices" ::
      { "memory" :: PParamRational
      , "cpu" :: PParamRational
      }
  , "maxExecutionUnitsPerTransaction" ::
      { "memory" :: BigNum
      , "cpu" :: BigNum
      }
  , "maxExecutionUnitsPerBlock" ::
      { "memory" :: BigNum
      , "cpu" :: BigNum
      }
  , "collateralPercentage" :: UInt
  , "maxCollateralInputs" :: UInt
  , "governanceActionDeposit" :: Maybe OgmiosAdaLovelace
  , "delegateRepresentativeDeposit" :: Maybe OgmiosAdaLovelace
  , "minFeeReferenceScripts" ::
      { range :: UInt
      , base :: Number
      , multiplier :: Number
      }
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
    minFeeReferenceScriptsBase <-
      note (TypeMismatch "minFeeReferenceScripts.multiplier: expected a number")
        $ Rational.fromNumber ps.minFeeReferenceScripts.base
    pure $ OgmiosProtocolParameters $ ProtocolParameters
      { protocolVersion: ps.version.major /\ ps.version.minor
      -- The following two parameters were removed from Babbage
      , decentralization: zero
      , maxBlockHeaderSize: ps.maxBlockHeaderSize.bytes
      , maxBlockBodySize: ps.maxBlockBodySize.bytes
      , maxTxSize: ps.maxTransactionSize.bytes
      , txFeeFixed: wrap ps.minFeeConstant.ada.lovelace
      , txFeePerByte: ps.minFeeCoefficient
      , stakeAddressDeposit: wrap ps.stakeCredentialDeposit.ada.lovelace
      , stakePoolDeposit: wrap ps.stakePoolDeposit.ada.lovelace
      , minPoolCost: wrap ps.minStakePoolCost.ada.lovelace
      , poolRetireMaxEpoch: wrap ps.stakePoolRetirementEpochBound
      , stakePoolTargetNum: ps.desiredNumberOfStakePools
      , poolPledgeInfluence: unwrap ps.stakePoolPledgeInfluence
      , monetaryExpansion: unwrap ps.monetaryExpansion
      , treasuryCut: unwrap ps.treasuryExpansion -- Rational
      , coinsPerUtxoByte: wrap ps.minUtxoDepositCoefficient
      , costModels: Map.fromFoldable $ catMaybes
          [ pure
              ( PlutusV1 /\ CostModel
                  ps.plutusCostModels."plutus:v1"
              )
          , Tuple PlutusV2 <<< CostModel <$>
              ps.plutusCostModels."plutus:v2"
          , Tuple PlutusV3 <<< CostModel <$>
              ps.plutusCostModels."plutus:v3"
          ]
      , prices: prices
      , maxTxExUnits: decodeExUnits ps.maxExecutionUnitsPerTransaction
      , maxBlockExUnits: decodeExUnits ps.maxExecutionUnitsPerBlock
      , maxValueSize: ps.maxValueSize.bytes
      , collateralPercent: ps.collateralPercentage
      , maxCollateralInputs: ps.maxCollateralInputs
      , govActionDeposit:
          -- NOTE: Conway fields should be optional to enable integration tests.
          -- Reason: cardano-testnet runs in the Babbage era.
          maybe mempty (wrap <<< _.ada.lovelace) ps.governanceActionDeposit
      , drepDeposit:
          maybe mempty (wrap <<< _.ada.lovelace)
            ps.delegateRepresentativeDeposit
      , refScriptCoinsPerByte: minFeeReferenceScriptsBase
      }
    where
    decodeExUnits
      :: { memory :: BigNum, cpu :: BigNum } -> ExUnits
    decodeExUnits { memory, cpu } = ExUnits { mem: memory, steps: cpu }

    decodePrices
      :: ProtocolParametersRaw -> Either JsonDecodeError ExUnitPrices
    decodePrices ps = note (TypeMismatch "ExUnitPrices") $ ExUnitPrices <$> do
      memPrice <- rationalToSubcoin ps.scriptExecutionPrices.memory
      stepPrice <- rationalToSubcoin ps.scriptExecutionPrices.cpu
      pure { memPrice, stepPrice } -- ExUnits

instance DecodeOgmios OgmiosProtocolParameters where
  decodeOgmios = decodeResult decodeAeson

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
  decodeOgmios = decodeResult decodeAeson

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
      encodeAeson
        { "language": "native"
        -- NOTE: We omit the cbor argument.
        , "json": (encodeNativeScript s)
        }
    encodeScriptRef (PlutusScriptRef (PlutusScript (script /\ lang))) =
      encodeAeson
        { "language":
            case lang of
              PlutusV1 -> "plutus:v1"
              PlutusV2 -> "plutus:v2"
              PlutusV3 -> "plutus:v3"
        , "cbor": byteArrayToHex script
        }

    encodeValue :: Value -> Aeson
    encodeValue value = encodeMap $ map encodeMap $ Map.union adaPart nonAdaPart
      where
      adaPart = Map.fromFoldable
        [ ( "ada" /\
              ( Map.fromFoldable
                  [ ("lovelace" /\ (value # valueToCoin # unwrap)) ]
              )
          )
        ]
      nonAdaPart = mapKeys (byteArrayToHex <<< unwrap <<< encodeCbor)
        $ map (mapKeys (byteArrayToHex <<< unAssetName))
        $ unwrap
        $ getMultiAsset value

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

-- Helper that decodes a null
aesonNull
  :: forall (a :: Type)
   . Aeson
  -> Either JsonDecodeError Unit
aesonNull = caseAesonNull (Left (TypeMismatch "Expected Null")) pure
