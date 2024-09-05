module Ctl.Internal.Service.Blockfrost
  ( BlockfrostChainTip(BlockfrostChainTip)
  , BlockfrostCurrentEpoch(BlockfrostCurrentEpoch)
  , BlockfrostRewards
  , BlockfrostEndpoint
      ( BlockchainGenesis
      , DatumCbor
      , EraSummaries
      , EvaluateTransaction
      , LatestBlock
      , LatestEpoch
      , LatestProtocolParameters
      , NativeScriptByHash
      , PlutusScriptCborByHash
      , ScriptInfo
      , SubmitTransaction
      , Transaction
      , TransactionMetadata
      , UtxosAtAddress
      , UtxosOfTransaction
      , PoolIds
      , PoolParameters
      , DelegationsAndRewards
      )
  , BlockfrostStakeCredential(BlockfrostStakeCredential)
  , BlockfrostEraSummaries(BlockfrostEraSummaries)
  , BlockfrostMetadata(BlockfrostMetadata)
  , BlockfrostNativeScript(BlockfrostNativeScript)
  , BlockfrostProtocolParameters(BlockfrostProtocolParameters)
  , BlockfrostRawPostResponseData
  , BlockfrostRawResponse
  , BlockfrostScriptInfo(BlockfrostScriptInfo)
  , BlockfrostScriptLanguage(NativeScript, PlutusV1Script, PlutusV2Script)
  , BlockfrostServiceM
  , BlockfrostServiceParams
  , BlockfrostSystemStart(BlockfrostSystemStart)
  , OnBlockfrostRawGetResponseHook
  , OnBlockfrostRawPostResponseHook
  , doesTxExist
  , evaluateTx
  , getChainTip
  , getCurrentEpoch
  , getDatumByHash
  , getEraSummaries
  , getOutputAddressesByTxHash
  , getPoolIds
  , getProtocolParameters
  , getPubKeyHashDelegationsAndRewards
  , getScriptByHash
  , getScriptInfo
  , getSystemStart
  , getTxAuxiliaryData
  , getTxMetadata
  , getUtxoByOref
  , getValidatorHashDelegationsAndRewards
  , runBlockfrostServiceM
  , runBlockfrostServiceTestM
  , submitTx
  , utxosAt
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch, MissingValue, AtKey)
  , decodeAeson
  , decodeJsonString
  , encodeAeson
  , getField
  , getFieldOptional
  , getFieldOptional'
  , isNull
  , parseJsonStringToAeson
  , stringifyAeson
  , (.:)
  , (.:!)
  )
import Affjax (Error, Response, URL, defaultRequest, printError) as Affjax
import Affjax.RequestBody (RequestBody, arrayView, string) as Affjax
import Affjax.RequestHeader (RequestHeader(ContentType, RequestHeader)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Serialization.Lib (toBytes)
import Cardano.Types
  ( AssetClass(AssetClass)
  , AuxiliaryData
  , DataHash
  , GeneralTransactionMetadata(GeneralTransactionMetadata)
  , Language(PlutusV3, PlutusV2, PlutusV1)
  , PlutusData
  , PoolPubKeyHash
  , RawBytes
  , RedeemerTag
  , ScriptHash
  , StakePubKeyHash
  , Transaction
  , TransactionHash
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , UnitInterval(UnitInterval)
  , UtxoMap
  , Value
  )
import Cardano.Types.Address (Address)
import Cardano.Types.Address as Address
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.CborBytes (CborBytes)
import Cardano.Types.Coin (Coin(Coin))
import Cardano.Types.Credential
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  )
import Cardano.Types.Epoch (Epoch(Epoch))
import Cardano.Types.ExUnitPrices (ExUnitPrices(ExUnitPrices))
import Cardano.Types.ExUnits (ExUnits(ExUnits))
import Cardano.Types.GeneralTransactionMetadata as GeneralTransactionMetadata
import Cardano.Types.Int (Int) as Cardano
import Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptAll
      , ScriptAny
      , ScriptNOfK
      , ScriptPubkey
      , TimelockExpiry
      , TimelockStart
      )
  )
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.OutputDatum (OutputDatum(OutputDatum, OutputDatumHash))
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.PoolPubKeyHash as PoolPubKeyHash
import Cardano.Types.RedeemerTag (RedeemerTag(Spend, Mint, Cert, Reward)) as RedeemerTag
import Cardano.Types.RewardAddress as RewardAddress
import Cardano.Types.ScriptRef (ScriptRef(NativeScriptRef, PlutusScriptRef))
import Cardano.Types.Value (assetToValue, lovelaceValueOf, sum) as Value
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Control.Monad.Logger.Class (log)
import Control.Monad.Logger.Trans (LoggerT(LoggerT), runLoggerT)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Parallel (parTraverse)
import Ctl.Internal.Affjax (request) as Affjax
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.Contract.QueryHandle.Error
  ( GetTxMetadataError
      ( GetTxMetadataTxNotFoundError
      , GetTxMetadataClientError
      , GetTxMetadataMetadataEmptyOrMissingError
      )
  )
import Ctl.Internal.QueryM.Ogmios
  ( AdditionalUtxoSet
  , ExecutionUnits
  , OgmiosDatum
  , OgmiosScript
  , OgmiosTxIn
  , RedeemerPointer
  , ScriptFailure
  , TxEvaluationFailure(ScriptFailures, UnparsedError)
  , TxEvaluationR
  , TxEvaluationResult(TxEvaluationResult)
  )
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Service.Error
  ( ClientError
      ( ClientDecodeJsonError
      , ClientHttpError
      , ClientHttpResponseError
      , ClientOtherError
      )
  , ServiceError(ServiceBlockfrostError)
  )
import Ctl.Internal.Service.Helpers
  ( aesonArray
  , aesonObject
  , aesonString
  , decodeAssetClass
  )
import Ctl.Internal.Types.Chain (Tip(Tip, TipAtGenesis))
import Ctl.Internal.Types.DelegationsAndRewards (DelegationsAndRewards)
import Ctl.Internal.Types.EraSummaries
  ( EraSummaries
  , EraSummary
  , EraSummaryParameters
  )
import Ctl.Internal.Types.ProtocolParameters
  ( CostModelV1
  , CostModelV2
  , ProtocolParameters(ProtocolParameters)
  , convertPlutusV1CostModel
  , convertPlutusV2CostModel
  , convertPlutusV3CostModel
  , convertUnnamedPlutusCostModel
  )
import Ctl.Internal.Types.Rational (Rational, reduce)
import Ctl.Internal.Types.StakeValidatorHash (StakeValidatorHash)
import Ctl.Internal.Types.SystemStart (SystemStart(SystemStart))
import Data.Array (catMaybes)
import Data.Array (find, length) as Array
import Data.Bifunctor (lmap)
import Data.BigNumber (BigNumber, toFraction)
import Data.BigNumber as BigNumber
import Data.ByteArray (ByteArray, byteArrayToHex)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(Left, Right), either, hush, note)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET, POST))
import Data.JSDate (JSDate, now)
import Data.Log.Level (LogLevel(Trace))
import Data.Log.Message (Message)
import Data.Map (Map)
import Data.Map (empty, fromFoldable, isEmpty) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.MediaType (MediaType(MediaType))
import Data.MediaType.Common (applicationJSON) as MediaType
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number (infinity)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern))
import Data.String (split, splitAt) as String
import Data.Time.Duration (Seconds(Seconds), convertDuration)
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt (fromString) as UInt
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (Object)
import Foreign.Object as ForeignObject
import JS.BigInt (fromString, toNumber) as BigInt
import Prim.TypeError (class Warn, Text)

--------------------------------------------------------------------------------
-- BlockfrostServiceM
--------------------------------------------------------------------------------

type BlockfrostRawResponse = String

type BlockfrostRawPostResponseData =
  { endpoint :: BlockfrostEndpoint
  , mediaType :: MediaType
  , requestBody :: Maybe Affjax.RequestBody
  , rawResponse :: BlockfrostRawResponse
  }

type OnBlockfrostRawGetResponseHook =
  Maybe (BlockfrostEndpoint -> BlockfrostRawResponse -> Aff Unit)

type OnBlockfrostRawPostResponseHook =
  Maybe (BlockfrostRawPostResponseData -> Aff Unit)

type BlockfrostServiceParams =
  { blockfrostConfig :: ServerConfig
  , blockfrostApiKey :: Maybe String
  , onBlockfrostRawGetResponse :: OnBlockfrostRawGetResponseHook
  , onBlockfrostRawPostResponse :: OnBlockfrostRawPostResponseHook
  }

type BlockfrostServiceM (a :: Type) = LoggerT
  (ReaderT BlockfrostServiceParams Aff)
  a

runBlockfrostServiceM
  :: forall (a :: Type)
   . (Message -> Aff Unit)
  -> BlockfrostBackend
  -> BlockfrostServiceM a
  -> Aff a
runBlockfrostServiceM logger backend =
  flip runReaderT (mkServiceParams Nothing Nothing backend) <<< flip runLoggerT
    (liftAff <<< logger)

runBlockfrostServiceTestM
  :: forall (a :: Type)
   . (Message -> Aff Unit)
  -> BlockfrostBackend
  -> OnBlockfrostRawGetResponseHook
  -> OnBlockfrostRawPostResponseHook
  -> BlockfrostServiceM a
  -> Aff a
runBlockfrostServiceTestM logger backend onRawGetResponse onRawPostResponse =
  flip runReaderT (mkServiceParams onRawGetResponse onRawPostResponse backend)
    <<< flip runLoggerT (liftAff <<< logger)

mkServiceParams
  :: OnBlockfrostRawGetResponseHook
  -> OnBlockfrostRawPostResponseHook
  -> BlockfrostBackend
  -> BlockfrostServiceParams
mkServiceParams onBlockfrostRawGetResponse onBlockfrostRawPostResponse backend =
  { blockfrostConfig: backend.blockfrostConfig
  , blockfrostApiKey: backend.blockfrostApiKey
  , onBlockfrostRawGetResponse
  , onBlockfrostRawPostResponse
  }

--------------------------------------------------------------------------------
-- Making requests to Blockfrost endpoints
--------------------------------------------------------------------------------

data BlockfrostEndpoint
  -- /genesis
  = BlockchainGenesis
  -- /scripts/datum/{datum_hash}/cbor
  | DatumCbor DataHash
  -- /network/eras
  | EraSummaries
  -- /utils/txs/evaluate/utxos
  | EvaluateTransaction
  -- /blocks/latest
  | LatestBlock
  -- /epochs/latest
  | LatestEpoch
  -- /epochs/latest/parameters
  | LatestProtocolParameters
  -- /scripts/{script_hash}/json
  | NativeScriptByHash ScriptHash
  -- /scripts/{script_hash}/cbor
  | PlutusScriptCborByHash ScriptHash
  -- /scripts/{script_hash}
  | ScriptInfo ScriptHash
  -- /tx/submit
  | SubmitTransaction
  -- /txs/{hash}
  | Transaction TransactionHash
  -- /txs/{hash}/metadata
  | TransactionMetadata TransactionHash
  -- /addresses/{address}/utxos?page={page}&count={count}
  | UtxosAtAddress Address Int Int
  -- /txs/{hash}/utxos
  | UtxosOfTransaction TransactionHash
  -- /pools?page={page}&count={count}&order=asc
  | PoolIds Int Int
  -- /pools/{hash}
  | PoolParameters PoolPubKeyHash
  -- /accounts/{stake_address}
  | DelegationsAndRewards BlockfrostStakeCredential

derive instance Generic BlockfrostEndpoint _
derive instance Eq BlockfrostEndpoint
derive instance Ord BlockfrostEndpoint

instance Show BlockfrostEndpoint where
  show = genericShow

realizeEndpoint :: BlockfrostEndpoint -> Affjax.URL
realizeEndpoint endpoint =
  case endpoint of
    BlockchainGenesis ->
      "/genesis"
    DatumCbor hash ->
      "/scripts/datum/" <> byteArrayToHex (unwrap $ encodeCbor hash) <> "/cbor"
    EraSummaries ->
      "/network/eras"
    EvaluateTransaction ->
      "/utils/txs/evaluate/utxos"
    LatestBlock ->
      "/blocks/latest"
    LatestEpoch ->
      "/epochs/latest"
    LatestProtocolParameters ->
      "/epochs/latest/parameters"
    NativeScriptByHash scriptHash ->
      "/scripts/" <> byteArrayToHex (unwrap $ encodeCbor scriptHash) <> "/json"
    PlutusScriptCborByHash scriptHash ->
      "/scripts/" <> byteArrayToHex (unwrap $ encodeCbor scriptHash) <> "/cbor"
    ScriptInfo scriptHash ->
      "/scripts/" <> byteArrayToHex (unwrap $ encodeCbor scriptHash)
    SubmitTransaction ->
      "/tx/submit"
    Transaction txHash ->
      "/txs/" <> byteArrayToHex (toBytes $ unwrap txHash)
    TransactionMetadata txHash ->
      "/txs/" <> byteArrayToHex (toBytes $ unwrap txHash) <> "/metadata/cbor"
    UtxosAtAddress address page count ->
      "/addresses/" <> Address.toBech32 address <> "/utxos?page=" <> show page
        <> ("&count=" <> show count)
    UtxosOfTransaction txHash ->
      "/txs/" <> byteArrayToHex (toBytes $ unwrap txHash) <> "/utxos"
    PoolIds page count ->
      "/pools?page=" <> show page <> "&count=" <> show count <> "&order=asc"
    PoolParameters poolPubKeyHash ->
      "/pool/" <> PoolPubKeyHash.toBech32 poolPubKeyHash
    DelegationsAndRewards credential ->
      "/accounts/" <> blockfrostStakeCredentialToBech32 credential

blockfrostGetRequest
  :: BlockfrostEndpoint
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
blockfrostGetRequest endpoint =
  withRequestResponseTracing
    (BlockfrostGetRequestData endpoint)
    ( ask >>= \params ->
        withOnRawGetResponseHook endpoint =<< liftAff do
          Affjax.request $ Affjax.defaultRequest
            { method = Left GET
            , url =
                mkHttpUrl params.blockfrostConfig <> realizeEndpoint endpoint
            , responseFormat = Affjax.ResponseFormat.string
            , headers =
                maybe mempty
                  (\apiKey -> [ Affjax.RequestHeader "project_id" apiKey ])
                  params.blockfrostApiKey
            }
    )

blockfrostPostRequest
  :: BlockfrostEndpoint
  -> MediaType
  -> Maybe Affjax.RequestBody
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
blockfrostPostRequest endpoint mediaType mbContent =
  withRequestResponseTracing
    (BlockfrostPostRequestData endpoint mediaType mbContent)
    ( ask >>= \params ->
        withOnRawPostResponseHook endpoint mediaType mbContent =<< liftAff do
          Affjax.request $ Affjax.defaultRequest
            { method = Left POST
            , url =
                mkHttpUrl params.blockfrostConfig <> realizeEndpoint endpoint
            , content = mbContent
            , responseFormat = Affjax.ResponseFormat.string
            , headers =
                [ Affjax.ContentType mediaType ] <>
                  maybe mempty
                    (\apiKey -> [ Affjax.RequestHeader "project_id" apiKey ])
                    params.blockfrostApiKey
            }
    )

withOnRawGetResponseHook
  :: BlockfrostEndpoint
  -> Either Affjax.Error (Affjax.Response String)
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
withOnRawGetResponseHook endpoint result = do
  for_ result \{ body: rawResponse } -> do
    onRawGetResponse <- asks _.onBlockfrostRawGetResponse
    liftAff $ for_ onRawGetResponse \f -> f endpoint rawResponse
  pure result

withOnRawPostResponseHook
  :: BlockfrostEndpoint
  -> MediaType
  -> Maybe Affjax.RequestBody
  -> Either Affjax.Error (Affjax.Response String)
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
withOnRawPostResponseHook endpoint mediaType requestBody result = do
  for_ result \{ body: rawResponse } -> do
    let data_ = { endpoint, mediaType, requestBody, rawResponse }
    onRawPostResponse <- asks _.onBlockfrostRawPostResponse
    liftAff $ for_ onRawPostResponse \f -> f data_
  pure result

data BlockfrostRequestData
  = BlockfrostGetRequestData BlockfrostEndpoint
  | BlockfrostPostRequestData BlockfrostEndpoint MediaType
      (Maybe Affjax.RequestBody)

withRequestResponseTracing
  :: BlockfrostRequestData
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
withRequestResponseTracing requestData performRequest = do
  timestamp <- liftEffect now
  trace timestamp requestMessage
  response <- performRequest
  trace timestamp (either Affjax.printError show response)
  pure response
  where
  trace :: JSDate -> String -> BlockfrostServiceM Unit
  trace timestamp message =
    log { level: Trace, message, tags: Map.empty, timestamp }

  requestMessage :: String
  requestMessage = case requestData of
    BlockfrostGetRequestData endpoint ->
      show { endpoint, url: realizeEndpoint endpoint }
    BlockfrostPostRequestData endpoint mediaType _ ->
      show
        { endpoint, mediaType {- mbContent -} , url: realizeEndpoint endpoint }

--------------------------------------------------------------------------------
-- Blockfrost response handling
--------------------------------------------------------------------------------

handleBlockfrostResponse
  :: forall (result :: Type)
   . DecodeAeson result
  => Either Affjax.Error (Affjax.Response String)
  -> Either ClientError result
handleBlockfrostResponse (Left affjaxError) =
  Left (ClientHttpError affjaxError)
handleBlockfrostResponse (Right { status: Affjax.StatusCode statusCode, body })
  | statusCode < 200 || statusCode > 299 = do
      blockfrostError <-
        body # lmap (ClientDecodeJsonError body)
          <<< (decodeAeson <=< parseJsonStringToAeson)
      Left $ ClientHttpResponseError (wrap statusCode) $
        ServiceBlockfrostError blockfrostError
  | otherwise =
      body # lmap (ClientDecodeJsonError body)
        <<< (decodeAeson <=< parseJsonStringToAeson)

handle404AsNothing
  :: forall (a :: Type)
   . Either ClientError (Maybe a)
  -> Either ClientError (Maybe a)
handle404AsNothing (Left (ClientHttpResponseError (Affjax.StatusCode 404) _)) =
  Right Nothing
handle404AsNothing x = x

handle404AsMempty
  :: forall (a :: Type)
   . Monoid a
  => Either ClientError (Maybe a)
  -> Either ClientError a
handle404AsMempty = map (fromMaybe mempty) <<< handle404AsNothing

--------------------------------------------------------------------------------
-- Get utxos at address / by output reference
--------------------------------------------------------------------------------

utxosAt :: Address -> BlockfrostServiceM (Either ClientError UtxoMap)
utxosAt address = runExceptT $
  ExceptT (utxosAtAddressOnPage 1)
    >>= (ExceptT <<< resolveBlockfrostUtxosAtAddress)
  where
  utxosAtAddressOnPage
    :: Int -> BlockfrostServiceM (Either ClientError BlockfrostUtxosAtAddress)
  utxosAtAddressOnPage page = runExceptT do
    -- Maximum number of results per page supported by Blockfrost:
    let maxNumResultsOnPage = 100
    utxos <- ExceptT $
      blockfrostGetRequest (UtxosAtAddress address page maxNumResultsOnPage)
        <#> handle404AsMempty <<< handleBlockfrostResponse
    case Array.length (unwrap utxos) < maxNumResultsOnPage of
      true -> pure utxos
      false -> append utxos <$> ExceptT (utxosAtAddressOnPage $ page + 1)

getUtxoByOref
  :: TransactionInput
  -> BlockfrostServiceM (Either ClientError (Maybe TransactionOutput))
getUtxoByOref oref@(TransactionInput { transactionId: txHash }) = runExceptT do
  (blockfrostUtxoMap :: BlockfrostUtxosOfTransaction) <- ExceptT $
    blockfrostGetRequest (UtxosOfTransaction txHash)
      <#> handle404AsMempty <<< handleBlockfrostResponse
  traverse (ExceptT <<< resolveBlockfrostTxOutput)
    (snd <$> Array.find (eq oref <<< fst) (unwrap blockfrostUtxoMap))

-- | Specialized function to get addresses only, without resolving script
-- | references. Used internally.
getOutputAddressesByTxHash
  :: TransactionHash
  -> BlockfrostServiceM (Either ClientError (Array Address))
getOutputAddressesByTxHash txHash = runExceptT do
  (blockfrostUtxoMap :: BlockfrostUtxosOfTransaction) <- ExceptT $
    blockfrostGetRequest (UtxosOfTransaction txHash)
      <#> handle404AsMempty <<< handleBlockfrostResponse
  pure $ _.address <<< unwrap <<< snd <$> unwrap blockfrostUtxoMap

--------------------------------------------------------------------------------
-- Get datum by hash
--------------------------------------------------------------------------------

getDatumByHash
  :: DataHash -> BlockfrostServiceM (Either ClientError (Maybe PlutusData))
getDatumByHash dataHash =
  blockfrostGetRequest (DatumCbor dataHash) <#> \response ->
    handle404AsNothing
      (unwrapBlockfrostDatum <$> handleBlockfrostResponse response)

--------------------------------------------------------------------------------
-- Get script by hash
--------------------------------------------------------------------------------

getScriptByHash
  :: ScriptHash
  -> BlockfrostServiceM (Either ClientError (Maybe ScriptRef))
getScriptByHash scriptHash = runExceptT $ runMaybeT do
  scriptInfo <- MaybeT $ ExceptT $ getScriptInfo scriptHash
  case scriptLanguage scriptInfo of
    NativeScript ->
      NativeScriptRef <$>
        MaybeT (ExceptT getNativeScriptByHash)
    PlutusV1Script ->
      PlutusScriptRef <<< PlutusScript.plutusV1Script <$>
        MaybeT (ExceptT getPlutusScriptCborByHash)
    PlutusV2Script ->
      PlutusScriptRef <<< PlutusScript.plutusV2Script <$>
        MaybeT (ExceptT getPlutusScriptCborByHash)
  where
  getNativeScriptByHash
    :: BlockfrostServiceM (Either ClientError (Maybe NativeScript))
  getNativeScriptByHash =
    blockfrostGetRequest (NativeScriptByHash scriptHash) <#> \response ->
      map unwrapBlockfrostNativeScript <$>
        handle404AsNothing (handleBlockfrostResponse response)

  getPlutusScriptCborByHash
    :: BlockfrostServiceM (Either ClientError (Maybe RawBytes))
  getPlutusScriptCborByHash =
    blockfrostGetRequest (PlutusScriptCborByHash scriptHash) <#> \response ->
      handle404AsNothing
        ( map wrap <<< unwrapBlockfrostCbor <$> handleBlockfrostResponse
            response
        )

getScriptInfo
  :: ScriptHash
  -> BlockfrostServiceM (Either ClientError (Maybe BlockfrostScriptInfo))
getScriptInfo scriptHash =
  blockfrostGetRequest (ScriptInfo scriptHash) <#> \response ->
    handle404AsNothing (handleBlockfrostResponse response)

--------------------------------------------------------------------------------
-- Submit / evaluate transaction
--------------------------------------------------------------------------------

submitTx
  :: Transaction
  -> BlockfrostServiceM (Either ClientError TransactionHash)
submitTx tx = do
  handleBlockfrostResponse <$> request (unwrap $ encodeCbor tx)
  where
  request
    :: ByteArray
    -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
  request cbor =
    blockfrostPostRequest SubmitTransaction (MediaType "application/cbor")
      (Just $ Affjax.arrayView $ unwrap cbor)

evaluateTx
  :: Transaction -> AdditionalUtxoSet -> BlockfrostServiceM TxEvaluationR
evaluateTx tx additionalUtxos = do
  resp <- handleBlockfrostResponse <$> request
  case unwrapBlockfrostEvaluateTx <$> resp of
    Left err -> throwError $ error $ show err
    Right (Left err) ->
      -- Replicate the error of QueryM's fault handler
      throwError $ error $ "Server responded with `fault`: " <> stringifyAeson
        err
    Right (Right eval) -> pure eval
  where
  request :: BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
  request = do
    blockfrostPostRequest EvaluateTransaction MediaType.applicationJSON
      ( Just $ Affjax.string $ stringifyAeson $
          encodeAeson
            { cbor: byteArrayToHex $ unwrap $ encodeCbor tx
            , additionalUtxoSet: additionalUtxos
            }
      )

--------------------------------------------------------------------------------
-- Check transaction confirmation status
--------------------------------------------------------------------------------

doesTxExist
  :: TransactionHash
  -> BlockfrostServiceM (Either ClientError Boolean)
doesTxExist txHash = do
  response <- blockfrostGetRequest $ Transaction txHash
  pure case handleBlockfrostResponse response of
    Right (_ :: Aeson) -> Right true
    Left (ClientHttpResponseError (Affjax.StatusCode 404) _) -> Right false
    Left e -> Left e

--------------------------------------------------------------------------------
-- Get transaction auxiliary data
getTxAuxiliaryData
  :: TransactionHash
  -> BlockfrostServiceM (Either GetTxMetadataError AuxiliaryData)
getTxAuxiliaryData txHash = runExceptT do
  metadata <- ExceptT $ getMetadata
  scriptRefs <- ExceptT $ getTxScripts txHash
  pure $ wrap
    { metadata: Just metadata
    , nativeScripts: arrayToMaybe $ getNativeScripts scriptRefs
    , plutusScripts: arrayToMaybe $ getPlutusScripts scriptRefs
    }

  where

  getMetadata = do
    response <- blockfrostGetRequest (TransactionMetadata txHash)
    pure case unwrapBlockfrostMetadata <$> handleBlockfrostResponse response of
      Left (ClientHttpResponseError (Affjax.StatusCode 404) _) ->
        Left GetTxMetadataTxNotFoundError
      Left e -> Left (GetTxMetadataClientError e)
      Right metadata
        | Map.isEmpty (unwrap metadata) ->
            Left GetTxMetadataMetadataEmptyOrMissingError
        | otherwise -> Right metadata

  getNativeScripts :: Array ScriptRef -> Array NativeScript
  getNativeScripts = catMaybes <<< map isNativeScript
    where
    isNativeScript (NativeScriptRef script) = Just script
    isNativeScript (PlutusScriptRef _) = Nothing

  getPlutusScripts :: Array ScriptRef -> Array PlutusScript
  getPlutusScripts = catMaybes <<< map isPlutusScript
    where
    isPlutusScript (PlutusScriptRef script) = Just script
    isPlutusScript (NativeScriptRef _) = Nothing

  arrayToMaybe :: forall a. Array a -> Maybe (Array a)
  arrayToMaybe [] = Nothing
  arrayToMaybe xs = Just xs

getTxMetadata
  :: Warn
       ( Text
           "deprecated: getTxMetadata. use Ctl.Internal.Service.Blockfrost.getTxAuxiliaryData"
       )
  => TransactionHash
  -> BlockfrostServiceM (Either GetTxMetadataError GeneralTransactionMetadata)
getTxMetadata txHash = do
  eAuxData <- getTxAuxiliaryData txHash
  pure $ case eAuxData of
    Left err -> Left err
    Right auxiliaryData -> case (unwrap auxiliaryData).metadata of
      Nothing -> Left GetTxMetadataMetadataEmptyOrMissingError
      Just metadata -> Right metadata

getTxScripts
  :: TransactionHash
  -> BlockfrostServiceM (Either GetTxMetadataError (Array ScriptRef))
getTxScripts txHash = runExceptT do
  (blockfrostUtxoMap :: BlockfrostUtxosOfTransaction) <- ExceptT $
    blockfrostGetRequest (UtxosOfTransaction txHash)
      <#> lmap GetTxMetadataClientError <<< handle404AsMempty <<<
        handleBlockfrostResponse
  let
    (scriptHashes :: Array ScriptHash) = catMaybes
      $ map (_.scriptHash <<< unwrap <<< snd)
      $ unwrap blockfrostUtxoMap
  catMaybes <$> traverse (ExceptT <<< scriptByHash) scriptHashes

  where
  scriptByHash t = (lmap GetTxMetadataClientError) <$> getScriptByHash t

--------------------------------------------------------------------------------
-- Get current epoch information
--------------------------------------------------------------------------------

getCurrentEpoch :: BlockfrostServiceM (Either ClientError BigNum)
getCurrentEpoch =
  blockfrostGetRequest LatestEpoch <#>
    handleBlockfrostResponse >>> map unwrapBlockfrostCurrentEpoch

getProtocolParameters
  :: BlockfrostServiceM (Either ClientError ProtocolParameters)
getProtocolParameters =
  blockfrostGetRequest LatestProtocolParameters <#>
    handleBlockfrostResponse >>> map unwrapBlockfrostProtocolParameters

--------------------------------------------------------------------------------
-- Get blockchain information
--------------------------------------------------------------------------------

getSystemStart :: BlockfrostServiceM (Either ClientError SystemStart)
getSystemStart = runExceptT do
  (systemStart :: BlockfrostSystemStart) <-
    ExceptT $ handleBlockfrostResponse <$>
      blockfrostGetRequest BlockchainGenesis
  pure $ unwrap systemStart

getChainTip :: BlockfrostServiceM (Either ClientError Tip)
getChainTip = runExceptT do
  (chainTip :: BlockfrostChainTip) <-
    ExceptT $ handleBlockfrostResponse <$> blockfrostGetRequest LatestBlock
  pure $ unwrap chainTip

getEraSummaries :: BlockfrostServiceM (Either ClientError EraSummaries)
getEraSummaries = runExceptT do
  (eraSummaries :: BlockfrostEraSummaries) <-
    ExceptT $ handleBlockfrostResponse <$> blockfrostGetRequest EraSummaries
  pure $ unwrap eraSummaries

--------------------------------------------------------------------------------
-- Staking pool IDs
--------------------------------------------------------------------------------

getPoolIds :: BlockfrostServiceM (Either ClientError (Array PoolPubKeyHash))
getPoolIds = runExceptT do
  ExceptT (poolsOnPage 1)
  where
  poolsOnPage
    :: Int -> BlockfrostServiceM (Either ClientError (Array PoolPubKeyHash))
  poolsOnPage page = runExceptT do
    let maxResultsOnPage = 100 -- blockfrost constant
    poolIds <- ExceptT $
      blockfrostGetRequest (PoolIds page maxResultsOnPage)
        <#> handle404AsMempty <<< handleBlockfrostResponse
    if Array.length poolIds < maxResultsOnPage then pure poolIds
    else append poolIds <$> ExceptT (poolsOnPage $ page + 1)

--------------------------------------------------------------------------------
-- Delegations and rewards
--------------------------------------------------------------------------------

getPubKeyHashDelegationsAndRewards
  :: NetworkId
  -> StakePubKeyHash
  -> BlockfrostServiceM (Either ClientError (Maybe DelegationsAndRewards))
getPubKeyHashDelegationsAndRewards networkId stakePubKeyHash = runExceptT do
  rewards <- ExceptT $
    blockfrostGetRequest
      ( DelegationsAndRewards $ BlockfrostStakeCredential networkId
          (Left stakePubKeyHash)
      )
      <#> handle404AsNothing <<< handleBlockfrostResponse
  pure $ rewards <#> \(BlockfrostRewards r) ->
    { rewards: r.withdrawable_amount
    , delegate: r.pool_id
    }

getValidatorHashDelegationsAndRewards
  :: NetworkId
  -> StakeValidatorHash
  -> BlockfrostServiceM (Either ClientError (Maybe DelegationsAndRewards))
getValidatorHashDelegationsAndRewards networkId stakeValidatorHash = runExceptT
  do
    rewards <- ExceptT $
      blockfrostGetRequest
        ( DelegationsAndRewards $ BlockfrostStakeCredential networkId
            (Right stakeValidatorHash)
        )
        <#> handle404AsNothing <<< handleBlockfrostResponse
    pure $ rewards <#> \(BlockfrostRewards r) ->
      { rewards: r.withdrawable_amount
      , delegate: r.pool_id
      }

--------------------------------------------------------------------------------
-- BlockfrostSystemStart
--------------------------------------------------------------------------------

newtype BlockfrostSystemStart = BlockfrostSystemStart SystemStart

derive instance Generic BlockfrostSystemStart _
derive instance Newtype BlockfrostSystemStart _

instance Show BlockfrostSystemStart where
  show = genericShow

instance DecodeAeson BlockfrostSystemStart where
  decodeAeson = aesonObject \obj -> do
    systemStart <- Seconds <<< BigInt.toNumber <$> getField obj "system_start"
    note (TypeMismatch "Unix timestamp")
      (wrap <<< wrap <<< toDateTime <$> instant (convertDuration systemStart))

--------------------------------------------------------------------------------
-- BlockfrostChainTip
--------------------------------------------------------------------------------

newtype BlockfrostChainTip = BlockfrostChainTip Tip

derive instance Generic BlockfrostChainTip _
derive instance Newtype BlockfrostChainTip _

instance Show BlockfrostChainTip where
  show = genericShow

instance DecodeAeson BlockfrostChainTip where
  decodeAeson = aesonObject \obj -> do
    blockHeaderHash <- wrap <$> getField obj "hash"
    getFieldOptional' obj "slot"
      <#> wrap
        <<< maybe TipAtGenesis (Tip <<< wrap <<< { blockHeaderHash, slot: _ })

--------------------------------------------------------------------------------
-- BlockfrostEraSummaries
--------------------------------------------------------------------------------

newtype BlockfrostEraSummaries = BlockfrostEraSummaries EraSummaries

derive instance Generic BlockfrostEraSummaries _
derive instance Newtype BlockfrostEraSummaries _

instance Show BlockfrostEraSummaries where
  show = genericShow

instance DecodeAeson BlockfrostEraSummaries where
  decodeAeson = aesonArray (map (wrap <<< wrap) <<< traverse decodeEraSummary)
    where
    decodeEraSummary :: Aeson -> Either JsonDecodeError EraSummary
    decodeEraSummary = aesonObject \obj -> do
      start <- getField obj "start"
      end <- getField obj "end"
      parameters <- decodeEraSummaryParameters =<< getField obj "parameters"
      pure $ wrap { start, end, parameters }

    decodeEraSummaryParameters
      :: Object Aeson -> Either JsonDecodeError EraSummaryParameters
    decodeEraSummaryParameters obj = do
      epochLength <- getField obj "epoch_length"
      slotLength <- wrap <$> mul slotLengthFactor <$> getField obj "slot_length"
      safeZone <- getField obj "safe_zone"
      pure $ wrap { epochLength, slotLength, safeZone }
      where
      -- Blockfrost returns `slotLength` in seconds, and we use milliseconds,
      -- so we need to convert between them.
      slotLengthFactor :: Number
      slotLengthFactor = 1000.0

--------------------------------------------------------------------------------
-- BlockfrostEvaluateTx
--------------------------------------------------------------------------------

data BlockfrostEvaluateTx = BlockfrostEvaluateTx (Either Aeson TxEvaluationR)

derive instance Generic BlockfrostEvaluateTx _

instance Show BlockfrostEvaluateTx where
  show = genericShow

instance DecodeAeson BlockfrostEvaluateTx where
  decodeAeson aeson = success <|> failure <#> BlockfrostEvaluateTx
    where
    success :: Either JsonDecodeError (Either Aeson TxEvaluationR)
    success = do
      { result: BlockfrostTxEvaluationR res }
        :: { result :: BlockfrostTxEvaluationR } <- decodeAeson aeson
      pure $ Right res

    failure :: Either JsonDecodeError (Either Aeson TxEvaluationR)
    failure = pure $ Left aeson

unwrapBlockfrostEvaluateTx :: BlockfrostEvaluateTx -> Either Aeson TxEvaluationR
unwrapBlockfrostEvaluateTx (BlockfrostEvaluateTx ei) = ei

--
-- TxEvaluationR parsing
--

-- | Wrapper for Aeson parsing.
--
-- Blockfrost returns on evaluateTx endpoint an ogmios response from the older Ogmios v5.6!
-- Ogmios backed parses against Ogmios v6, here we parse using the previous code for Ogmios.
--
-- Note: TxEvaluationFailure as part of BlockfrostTxEvaluation doesn't parse with it's DecodeAeson instance.
newtype BlockfrostTxEvaluationR = BlockfrostTxEvaluationR TxEvaluationR

instance DecodeAeson BlockfrostTxEvaluationR where
  decodeAeson aeson = BlockfrostTxEvaluationR <$>
    ( (wrap <<< Right <$> decodeBlockfrostTxEvaluationResult aeson) <|>
        (wrap <<< Left <$> decodeBlockfrostTxEvaluationFailure aeson)
    )

decodeBlockfrostTxEvaluationResult
  :: Aeson -> Either JsonDecodeError TxEvaluationResult
decodeBlockfrostTxEvaluationResult = aesonObject $ \obj -> do
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

decodeRedeemerPointer :: String -> Either JsonDecodeError RedeemerPointer
decodeRedeemerPointer redeemerPtrRaw = note redeemerPtrTypeMismatch
  case String.split (Pattern ":") redeemerPtrRaw of
    [ tagRaw, indexRaw ] ->
      { redeemerTag: _, redeemerIndex: _ }
        <$> redeemerTagFromString tagRaw
        <*> UInt.fromString indexRaw
    _ -> Nothing

redeemerTagFromString :: String -> Maybe RedeemerTag
redeemerTagFromString = case _ of
  "spend" -> Just RedeemerTag.Spend
  "mint" -> Just RedeemerTag.Mint
  "certificate" -> Just RedeemerTag.Cert
  "withdrawal" -> Just RedeemerTag.Reward
  _ -> Nothing

redeemerPtrTypeMismatch :: JsonDecodeError
redeemerPtrTypeMismatch = TypeMismatch
  "Expected redeemer pointer to be encoded as: \
  \^(spend|mint|certificate|withdrawal):[0-9]+$"

data OldScriptFailure
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

type ObjectParser = ReaderT (Object Aeson) (Either JsonDecodeError)

liftField
  :: forall (a :: Type) (b :: Type)
   . DecodeAeson a
  => String
  -> (a -> Either JsonDecodeError b)
  -> ObjectParser b
liftField f act = ReaderT (flip getField f >=> act)

instance DecodeAeson OldScriptFailure where
  decodeAeson = aesonObject $ runReaderT cases
    where
    cases :: ObjectParser OldScriptFailure
    cases = decodeExtraRedeemers
      <|> decodeMissingRequiredDatums
      <|> decodeMissingRequiredScripts
      <|> decodeValidatorFailed
      <|> decodeUnknownInputReferencedByRedeemer
      <|> decodeNonScriptInputReferencedByRedeemer
      <|> decodeIllFormedExecutionBudget
      <|> decodeNoCostModelForLanguage
      <|> defaultCase

    defaultCase :: ObjectParser OldScriptFailure
    defaultCase = ReaderT $ const $ Left $ TypeMismatch "Expected ScriptFailure"

    decodeExtraRedeemers :: ObjectParser OldScriptFailure
    decodeExtraRedeemers = ExtraRedeemers <$> liftField "extraRedeemers"
      (traverse decodeRedeemerPointer)

    decodeMissingRequiredDatums :: ObjectParser OldScriptFailure
    decodeMissingRequiredDatums = liftField "missingRequiredDatums" \o -> do
      pure $ MissingRequiredDatums o

    decodeMissingRequiredScripts :: ObjectParser OldScriptFailure
    decodeMissingRequiredScripts = liftField "missingRequiredScripts" \o -> do
      resolvedKV <- ForeignObject.toUnfoldable <$> getField o "resolved"
      resolved <- Map.fromFoldable <$> for (resolvedKV :: Array _)
        \(k /\ v) -> (_ /\ v) <$> decodeRedeemerPointer k
      missing <- getField o "missing"
      pure $ MissingRequiredScripts { resolved, missing }

    decodeValidatorFailed :: ObjectParser OldScriptFailure
    decodeValidatorFailed = liftField "validatorFailed" \o -> do
      pure $ ValidatorFailed o

    decodeUnknownInputReferencedByRedeemer :: ObjectParser OldScriptFailure
    decodeUnknownInputReferencedByRedeemer = liftField
      "unknownInputReferencedByRedeemer"
      \o -> do
        pure $ UnknownInputReferencedByRedeemer o

    decodeNonScriptInputReferencedByRedeemer :: ObjectParser OldScriptFailure
    decodeNonScriptInputReferencedByRedeemer = liftField
      "nonScriptInputReferencedByRedeemer"
      \o -> do
        pure $ NonScriptInputReferencedByRedeemer o

    decodeIllFormedExecutionBudget :: ObjectParser OldScriptFailure
    decodeIllFormedExecutionBudget = liftField "illFormedExecutionBudget" \o ->
      do
        pure $ IllFormedExecutionBudget o

    decodeNoCostModelForLanguage :: ObjectParser OldScriptFailure
    decodeNoCostModelForLanguage = liftField "noCostModelForLanguage" \o -> do
      pure $ NoCostModelForLanguage o

decodeBlockfrostTxEvaluationFailure
  :: Aeson -> Either JsonDecodeError TxEvaluationFailure
decodeBlockfrostTxEvaluationFailure = aesonObject $ runReaderT cases
  where
  cases :: ObjectParser TxEvaluationFailure
  cases = decodeScriptFailures <|> defaultCase

  defaultCase :: ObjectParser TxEvaluationFailure
  defaultCase = ReaderT \o ->
    pure (UnparsedError (stringifyAeson (encodeAeson o)))

  -- translate Ogmios v5.6 ScriptFailures to Ogmios v6
  translateOldToNew :: OldScriptFailure -> Either JsonDecodeError ScriptFailure
  translateOldToNew x = case x of
    ExtraRedeemers ptrs -> pure $ Ogmios.ExtraRedeemers ptrs
    MissingRequiredDatums { provided, missing } -> pure $
      Ogmios.MissingRequiredDatums { missing, provided: provided }
    MissingRequiredScripts { resolved: resolved0, missing: missing0 } -> do
      missing <- traverse decodeRedeemerPointer missing0
      resolved :: Map RedeemerPointer ScriptHash <- traverse decodeAeson
        (map (encodeAeson :: String -> _) resolved0)
      pure $ Ogmios.MissingRequiredScripts { missing, resolved: Just resolved }
    ValidatorFailed { error, traces } -> pure $ Ogmios.ValidatorFailed
      { error, traces }
    UnknownInputReferencedByRedeemer txin -> pure $
      Ogmios.UnknownInputReferencedByRedeemer [ txin ]
    NonScriptInputReferencedByRedeemer txin -> pure $
      Ogmios.NonScriptInputReferencedByRedeemer txin
    IllFormedExecutionBudget mexu -> pure $
      Ogmios.IllFormedExecutionBudget mexu
    NoCostModelForLanguage lang -> pure $ Ogmios.NoCostModelForLanguage [ lang ]

  decodeScriptFailures :: ObjectParser TxEvaluationFailure
  decodeScriptFailures = ReaderT \o -> do
    scriptFailuresKV <- ForeignObject.toUnfoldable
      <$> (getField o "EvaluationFailure" >>= flip getField "ScriptFailures")
    scriptFailures <- Map.fromFoldable <$> for (scriptFailuresKV :: Array _)
      \(k /\ v) -> do
        v' <- traverse translateOldToNew =<< decodeAeson v
        (_ /\ v') <$> decodeRedeemerPointer k
    pure $ ScriptFailures scriptFailures

--------------------------------------------------------------------------------
-- BlockfrostUtxosAtAddress / BlockfrostUtxosOfTransaction
--------------------------------------------------------------------------------

type BlockfrostUnspentOutput = TransactionInput /\ BlockfrostTransactionOutput

newtype BlockfrostUtxosAtAddress =
  BlockfrostUtxosAtAddress (Array BlockfrostUnspentOutput)

derive instance Generic BlockfrostUtxosAtAddress _
derive instance Newtype BlockfrostUtxosAtAddress _
derive newtype instance Semigroup BlockfrostUtxosAtAddress
derive newtype instance Monoid BlockfrostUtxosAtAddress

instance Show BlockfrostUtxosAtAddress where
  show = genericShow

instance DecodeAeson BlockfrostUtxosAtAddress where
  decodeAeson = aesonArray (map wrap <<< traverse decodeUtxoEntry)
    where
    decodeUtxoEntry :: Aeson -> Either JsonDecodeError BlockfrostUnspentOutput
    decodeUtxoEntry utxoAeson =
      Tuple <$> decodeTxOref utxoAeson <*> decodeAeson utxoAeson

    decodeTxOref :: Aeson -> Either JsonDecodeError TransactionInput
    decodeTxOref = aesonObject \obj -> do
      transactionId <- getField obj "tx_hash"
      index <- getField obj "output_index"
      pure $ TransactionInput { transactionId, index }

resolveBlockfrostUtxosAtAddress
  :: BlockfrostUtxosAtAddress
  -> BlockfrostServiceM (Either ClientError UtxoMap)
resolveBlockfrostUtxosAtAddress (BlockfrostUtxosAtAddress utxos) =
  -- TODO: `Parallel` instance for `BlockfrostServiceM`?
  LoggerT \logger ->
    let
      resolve
        :: BlockfrostTransactionOutput
        -> ExceptT ClientError (ReaderT BlockfrostServiceParams Aff)
             TransactionOutput
      resolve = ExceptT <<< flip runLoggerT logger <<< resolveBlockfrostTxOutput
    in
      runExceptT $ Map.fromFoldable <$> parTraverse (traverse resolve) utxos

newtype BlockfrostUtxosOfTransaction =
  BlockfrostUtxosOfTransaction (Array BlockfrostUnspentOutput)

derive instance Generic BlockfrostUtxosOfTransaction _
derive instance Newtype BlockfrostUtxosOfTransaction _
derive newtype instance Semigroup BlockfrostUtxosOfTransaction
derive newtype instance Monoid BlockfrostUtxosOfTransaction

instance Show BlockfrostUtxosOfTransaction where
  show = genericShow

instance DecodeAeson BlockfrostUtxosOfTransaction where
  decodeAeson = aesonObject \obj -> do
    txHash <- getField obj "hash"
    getField obj "outputs"
      >>= aesonArray (map wrap <<< traverse (decodeUtxoEntry txHash))
    where
    decodeUtxoEntry
      :: TransactionHash
      -> Aeson
      -> Either JsonDecodeError BlockfrostUnspentOutput
    decodeUtxoEntry txHash utxoAeson =
      Tuple <$> decodeTxOref txHash utxoAeson <*> decodeAeson utxoAeson

    decodeTxOref
      :: TransactionHash -> Aeson -> Either JsonDecodeError TransactionInput
    decodeTxOref txHash = aesonObject $
      flip getField "output_index" >>> map \index ->
        TransactionInput { transactionId: txHash, index }

--------------------------------------------------------------------------------
-- BlockfrostTransactionOutput
--------------------------------------------------------------------------------

newtype BlockfrostTransactionOutput = BlockfrostTransactionOutput
  { address :: Address
  , amount :: Value
  , datum :: Maybe OutputDatum
  , scriptHash :: Maybe ScriptHash
  }

derive instance Generic BlockfrostTransactionOutput _
derive instance Newtype BlockfrostTransactionOutput _

instance Show BlockfrostTransactionOutput where
  show = genericShow

instance DecodeAeson BlockfrostTransactionOutput where
  decodeAeson = aesonObject \obj -> do
    address <- decodeAddress obj
    amount <- decodeValue obj
    datum <- decodeOutputDatum obj
    scriptHash <- getFieldOptional' obj "reference_script_hash"
    pure $ wrap { address, amount, datum, scriptHash }
    where
    decodeAddress :: Object Aeson -> Either JsonDecodeError Address
    decodeAddress obj =
      getField obj "address" >>= \address ->
        note (TypeMismatch "Expected bech32 encoded address")
          (Address.fromBech32 address)

    decodeValue :: Object Aeson -> Either JsonDecodeError Value
    decodeValue =
      flip getField "amount"
        >=> aesonArray (traverse decodeAsset)
        >=>
          Value.sum >>> note (TypeMismatch "Value")
      where

      decodeAsset :: Aeson -> Either JsonDecodeError Value
      decodeAsset = aesonObject \obj -> do
        quantity <-
          getField obj "quantity" >>=
            BigNum.fromString >>>
              note (TypeMismatch "Expected string repr of BigInt")
        getField obj "unit" >>= case _ of
          "lovelace" -> pure $ Value.lovelaceValueOf quantity
          assetString -> do
            let { before: csStr, after: tnStr } = String.splitAt 56 assetString
            decodeAssetClass assetString csStr tnStr <#> \(cs /\ tn) ->
              Value.assetToValue (AssetClass cs tn) quantity

    decodeOutputDatum
      :: Object Aeson -> Either JsonDecodeError (Maybe OutputDatum)
    decodeOutputDatum obj =
      getFieldOptional' obj "inline_datum" >>= case _ of
        Just datum ->
          note (TypeMismatch "OutputDatum")
            (Just <<< OutputDatum <$> decodeCbor datum)
        Nothing ->
          pure $ join $ map OutputDatumHash <$> hush
            (getFieldOptional' obj "data_hash")

resolveBlockfrostTxOutput
  :: BlockfrostTransactionOutput
  -> BlockfrostServiceM (Either ClientError TransactionOutput)
resolveBlockfrostTxOutput
  (BlockfrostTransactionOutput blockfrostTxOutput@{ address, amount, datum }) =
  map mkTxOutput <$> resolveScriptRef
  where
  mkTxOutput :: Maybe ScriptRef -> TransactionOutput
  mkTxOutput scriptRef =
    TransactionOutput { address, amount, datum, scriptRef }

  resolveScriptRef :: BlockfrostServiceM (Either ClientError (Maybe ScriptRef))
  resolveScriptRef = runExceptT do
    for blockfrostTxOutput.scriptHash \scriptHash -> do
      scriptRef <- ExceptT $ getScriptByHash scriptHash
      flip liftMaybe scriptRef
        (ClientOtherError "Blockfrost: Failed to resolve reference script")

--------------------------------------------------------------------------------
-- BlockfrostScriptLanguage
--------------------------------------------------------------------------------

data BlockfrostScriptLanguage = NativeScript | PlutusV1Script | PlutusV2Script

derive instance Generic BlockfrostScriptLanguage _
derive instance Eq BlockfrostScriptLanguage

instance Show BlockfrostScriptLanguage where
  show = genericShow

instance DecodeAeson BlockfrostScriptLanguage where
  decodeAeson = aesonString $ case _ of
    "timelock" -> pure NativeScript
    "plutusV1" -> pure PlutusV1Script
    "plutusV2" -> pure PlutusV2Script
    invalid ->
      Left $ TypeMismatch $
        "language: expected 'native' or 'plutusV{1|2}', got: " <> invalid

--------------------------------------------------------------------------------
-- BlockfrostScriptInfo
--------------------------------------------------------------------------------

newtype BlockfrostScriptInfo = BlockfrostScriptInfo
  { language :: BlockfrostScriptLanguage
  }

scriptLanguage :: BlockfrostScriptInfo -> BlockfrostScriptLanguage
scriptLanguage = _.language <<< unwrap

derive instance Generic BlockfrostScriptInfo _
derive instance Newtype BlockfrostScriptInfo _
derive instance Eq BlockfrostScriptInfo

instance Show BlockfrostScriptInfo where
  show = genericShow

instance DecodeAeson BlockfrostScriptInfo where
  decodeAeson =
    aesonObject (map (wrap <<< { language: _ }) <<< flip getField "type")

--------------------------------------------------------------------------------
-- BlockfrostNativeScript
--------------------------------------------------------------------------------

newtype BlockfrostNativeScript = BlockfrostNativeScript NativeScript

derive instance Generic BlockfrostNativeScript _
derive instance Newtype BlockfrostNativeScript _

unwrapBlockfrostNativeScript :: BlockfrostNativeScript -> NativeScript
unwrapBlockfrostNativeScript = unwrap

instance Show BlockfrostNativeScript where
  show = genericShow

instance DecodeAeson BlockfrostNativeScript where
  decodeAeson =
    aesonObject (flip getField "json") >=> (map wrap <<< decodeNativeScript)
    where
    decodeNativeScript :: Object Aeson -> Either JsonDecodeError NativeScript
    decodeNativeScript obj = getField obj "type" >>= case _ of
      "sig" ->
        ScriptPubkey <$>
          ( getField obj "keyHash" >>=
              (note (TypeMismatch "Ed25519KeyHash") <<< decodeCbor)
          )
      "before" ->
        TimelockExpiry <$> getField obj "slot"
      "after" ->
        TimelockStart <$> getField obj "slot"
      "all" ->
        ScriptAll <$> decodeScripts
      "any" ->
        ScriptAny <$> decodeScripts
      "atLeast" ->
        ScriptNOfK <$> getField obj "required" <*> decodeScripts
      _ ->
        Left $ TypeMismatch "Native script constructor"
      where
      decodeScripts :: Either JsonDecodeError (Array NativeScript)
      decodeScripts =
        getField obj "scripts" >>= traverse (aesonObject decodeNativeScript)

--------------------------------------------------------------------------------
-- BlockfrostCbor
--------------------------------------------------------------------------------

newtype BlockfrostCbor = BlockfrostCbor (Maybe ByteArray)

derive instance Generic BlockfrostCbor _
derive instance Newtype BlockfrostCbor _

unwrapBlockfrostCbor :: BlockfrostCbor -> Maybe ByteArray
unwrapBlockfrostCbor = unwrap

instance Show BlockfrostCbor where
  show = genericShow

instance DecodeAeson BlockfrostCbor where
  decodeAeson aeson
    | isNull aeson = pure $ BlockfrostCbor Nothing
    | otherwise = do
        cbor <- aesonObject (flip getFieldOptional "cbor") aeson
        pure $ BlockfrostCbor cbor

--------------------------------------------------------------------------------
-- BlockfrostDatum
--------------------------------------------------------------------------------

newtype BlockfrostDatum = BlockfrostDatum (Maybe PlutusData)

derive instance Generic BlockfrostDatum _
derive instance Newtype BlockfrostDatum _

instance Show BlockfrostDatum where
  show = genericShow

unwrapBlockfrostDatum :: BlockfrostDatum -> Maybe PlutusData
unwrapBlockfrostDatum = unwrap

instance DecodeAeson BlockfrostDatum where
  decodeAeson aeson
    | isNull aeson = pure $ BlockfrostDatum Nothing
    | otherwise = do
        cbor <- aesonObject (flip getFieldOptional "cbor") aeson
        pure $ BlockfrostDatum $ decodeCbor =<< cbor

--------------------------------------------------------------------------------
-- BlockfrostMetadata
--------------------------------------------------------------------------------

newtype BlockfrostMetadata = BlockfrostMetadata GeneralTransactionMetadata

derive instance Generic BlockfrostMetadata _
derive instance Eq BlockfrostMetadata
derive instance Newtype BlockfrostMetadata _

instance Show BlockfrostMetadata where
  show = genericShow

instance DecodeAeson BlockfrostMetadata where
  decodeAeson = decodeAeson >=>
    \(metadatas :: Array { metadata :: CborBytes }) -> do
      metadatas' <- for metadatas \{ metadata } -> do
        decodeCbor metadata # note
          (TypeMismatch "Hexadecimal encoded Metadata")
      note (TypeMismatch "GeneralTransactionMetadata") $
        BlockfrostMetadata <$> GeneralTransactionMetadata.fold metadatas'

unwrapBlockfrostMetadata :: BlockfrostMetadata -> GeneralTransactionMetadata
unwrapBlockfrostMetadata (BlockfrostMetadata metadata) = metadata

--------------------------------------------------------------------------------
-- BlockfrostCurrentEpoch
--------------------------------------------------------------------------------

newtype BlockfrostCurrentEpoch = BlockfrostCurrentEpoch BigNum

derive instance Generic BlockfrostCurrentEpoch _
derive instance Newtype BlockfrostCurrentEpoch _

instance Show BlockfrostCurrentEpoch where
  show = genericShow

instance DecodeAeson BlockfrostCurrentEpoch where
  decodeAeson a = decodeAeson a <#>
    \({ epoch } :: { epoch :: BigNum }) -> wrap epoch

unwrapBlockfrostCurrentEpoch :: BlockfrostCurrentEpoch -> BigNum
unwrapBlockfrostCurrentEpoch = unwrap

data BlockfrostStakeCredential = BlockfrostStakeCredential NetworkId
  (Either StakePubKeyHash StakeValidatorHash)

derive instance Generic BlockfrostStakeCredential _

derive instance Eq BlockfrostStakeCredential
derive instance Ord BlockfrostStakeCredential

instance Show BlockfrostStakeCredential where
  show = genericShow

blockfrostStakeCredentialToBech32 :: BlockfrostStakeCredential -> Bech32String
blockfrostStakeCredentialToBech32 = case _ of
  BlockfrostStakeCredential networkId (Left stakePubKeyHash) ->
    RewardAddress.toBech32 $
      { networkId
      , stakeCredential: wrap $ PubKeyHashCredential $ unwrap stakePubKeyHash
      }
  BlockfrostStakeCredential networkId (Right stakeValidatorHash) ->
    RewardAddress.toBech32 $
      { networkId
      , stakeCredential: wrap $ ScriptHashCredential $ unwrap stakeValidatorHash
      }

--------------------------------------------------------------------------------
-- BlockfrostProtocolParameters
--------------------------------------------------------------------------------

-- | `Stringed a` decodes an `a` that was encoded as a `String`
newtype Stringed a = Stringed a

derive instance Newtype (Stringed a) _

instance DecodeAeson a => DecodeAeson (Stringed a) where
  decodeAeson = decodeAeson >=> decodeJsonString >=> Stringed >>> pure

newtype FiniteBigNumber = FiniteBigNumber BigNumber

derive instance Newtype FiniteBigNumber _

instance DecodeAeson FiniteBigNumber where
  decodeAeson aeson = do
    number <- decodeAeson aeson
    map FiniteBigNumber $ note (TypeMismatch "BigNumber") $ hush
      $ BigNumber.parseBigNumber
      $ show (number :: Number)

type BlockfrostProtocolParametersRaw =
  { "min_fee_a" :: UInt
  , "min_fee_b" :: UInt
  , "max_block_size" :: UInt
  , "max_tx_size" :: UInt
  , "max_block_header_size" :: UInt
  , "key_deposit" :: Stringed BigNum
  , "pool_deposit" :: Stringed BigNum
  , "e_max" :: UInt
  , "n_opt" :: UInt
  , "a0" :: FiniteBigNumber
  , "rho" :: FiniteBigNumber
  , "tau" :: FiniteBigNumber
  , "protocol_major_ver" :: UInt
  , "protocol_minor_ver" :: UInt
  , "min_pool_cost" :: Stringed BigNum
  , "cost_models" ::
      { "PlutusV1" :: { | CostModelV1 }
      , "PlutusV2" :: { | CostModelV2 }
      , "PlutusV3" :: Object Cardano.Int
      }
  , "price_mem" :: FiniteBigNumber
  , "price_step" :: FiniteBigNumber
  , "max_tx_ex_mem" :: Stringed BigNum
  , "max_tx_ex_steps" :: Stringed BigNum
  , "max_block_ex_mem" :: Stringed BigNum
  , "max_block_ex_steps" :: Stringed BigNum
  , "max_val_size" :: Stringed UInt
  , "collateral_percent" :: UInt
  , "max_collateral_inputs" :: UInt
  , "coins_per_utxo_size" :: Maybe (Stringed BigNum)
  , "gov_action_deposit" :: Stringed BigNum
  , "drep_deposit" :: Stringed BigNum
  }

toFraction' :: BigNumber -> String /\ String
toFraction' bn =
  (BigNumber.toString numerator /\ BigNumber.toString denominator)
  where
  (numerator /\ denominator) = toFraction bn
    (BigNumber.fromNumber infinity)

bigNumberToRational :: FiniteBigNumber -> Either JsonDecodeError Rational
bigNumberToRational (FiniteBigNumber bn) = note (TypeMismatch "Rational") do
  numerator <- BigInt.fromString numerator'
  denominator <- BigInt.fromString denominator'
  reduce numerator denominator
  where
  (numerator' /\ denominator') = toFraction' bn

bigNumberToPrice
  :: FiniteBigNumber
  -> Either JsonDecodeError UnitInterval
bigNumberToPrice (FiniteBigNumber bn) = note (TypeMismatch "Rational") do
  numerator <- BigNum.fromString numerator'
  denominator <- BigNum.fromString denominator'
  pure $ UnitInterval { numerator, denominator }
  where
  (numerator' /\ denominator') = toFraction' bn

newtype BlockfrostProtocolParameters =
  BlockfrostProtocolParameters ProtocolParameters

derive instance Generic BlockfrostProtocolParameters _
derive instance Newtype BlockfrostProtocolParameters _

unwrapBlockfrostProtocolParameters
  :: BlockfrostProtocolParameters -> ProtocolParameters
unwrapBlockfrostProtocolParameters = unwrap

instance Show BlockfrostProtocolParameters where
  show = genericShow

instance DecodeAeson BlockfrostProtocolParameters where
  decodeAeson = decodeAeson >=> \(raw :: BlockfrostProtocolParametersRaw) -> do
    poolPledgeInfluence <- bigNumberToRational raw.a0
    monetaryExpansion <- bigNumberToRational raw.rho
    treasuryCut <- bigNumberToRational raw.tau
    memPrice <- bigNumberToPrice raw.price_mem
    stepPrice <- bigNumberToPrice raw.price_step
    let prices = ExUnitPrices { memPrice, stepPrice }

    coinsPerUtxoByte <-
      maybe (Left $ AtKey "coins_per_utxo_size" $ MissingValue)
        pure $ (Coin <<< unwrap <$> raw.coins_per_utxo_size)

    let plutusV3CostModelRaw = raw.cost_models."PlutusV3"
    plutusV3CostModel <-
      note (AtKey "cost_models" $ AtKey "PlutusV3" $ TypeMismatch "CostModel")
        ( convertPlutusV3CostModel plutusV3CostModelRaw
            <|> convertUnnamedPlutusCostModel plutusV3CostModelRaw
        )
    pure $ BlockfrostProtocolParameters $ ProtocolParameters
      { protocolVersion: raw.protocol_major_ver /\ raw.protocol_minor_ver
      -- The following two parameters were removed from Babbage
      , decentralization: zero
      , maxBlockHeaderSize: raw.max_block_header_size
      , maxBlockBodySize: raw.max_block_size
      , maxTxSize: raw.max_tx_size
      , txFeeFixed: Coin $ BigNum.fromUInt raw.min_fee_b
      , txFeePerByte: raw.min_fee_a
      , stakeAddressDeposit: Coin $ unwrap raw.key_deposit
      , stakePoolDeposit: Coin $ unwrap raw.pool_deposit
      , minPoolCost: Coin $ unwrap raw.min_pool_cost
      , poolRetireMaxEpoch: Epoch raw.e_max
      , stakePoolTargetNum: raw.n_opt
      , poolPledgeInfluence
      , monetaryExpansion
      , treasuryCut
      , coinsPerUtxoByte: coinsPerUtxoByte
      , costModels: Map.fromFoldable
          [ PlutusV1 /\ convertPlutusV1CostModel raw.cost_models."PlutusV1"
          , PlutusV2 /\ convertPlutusV2CostModel raw.cost_models."PlutusV2"
          , PlutusV3 /\ plutusV3CostModel
          ]
      , prices
      , maxTxExUnits:
          ExUnits
            { mem: unwrap raw.max_tx_ex_mem
            , steps: unwrap raw.max_tx_ex_steps
            }
      , maxBlockExUnits:
          ExUnits
            { mem: unwrap raw.max_block_ex_mem
            , steps: unwrap raw.max_block_ex_steps
            }
      , maxValueSize: unwrap raw.max_val_size
      , collateralPercent: raw.collateral_percent
      , maxCollateralInputs: raw.max_collateral_inputs
      , govActionDeposit: Coin $ unwrap raw.gov_action_deposit
      , drepDeposit: Coin $ unwrap raw.drep_deposit
      }

--------------------------------------------------------------------------------
-- BlockfrostRewards
--------------------------------------------------------------------------------

newtype BlockfrostRewards = BlockfrostRewards
  { pool_id :: Maybe PoolPubKeyHash
  , withdrawable_amount :: Maybe Coin
  }

instance DecodeAeson BlockfrostRewards where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    pool_id <- obj .: "pool_id"
    withdrawable_amount_mb_str <- obj .:! "withdrawable_amount"
    withdrawable_amount <- for withdrawable_amount_mb_str
      \withdrawable_amount_str ->
        note (TypeMismatch "BigNum") $ map Coin $ BigNum.fromString
          withdrawable_amount_str
    pure $ BlockfrostRewards
      { pool_id
      , withdrawable_amount
      }
