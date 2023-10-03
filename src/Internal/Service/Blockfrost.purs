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
      , AssetAddresses
      , AssetsOfPolicy
      , AssetUtxosAtAddress
      , AssetTransactions
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
  , getTxMetadata
  , getUtxoByOref
  , getValidatorHashDelegationsAndRewards
  , runBlockfrostServiceM
  , runBlockfrostServiceTestM
  , submitTx
  , utxosAt
  , utxosWithAssetClass
  , utxosWithCurrencySymbol
  , utxosInTransaction
  , allOutputsWithCurrencySymbol
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , Finite
  , JsonDecodeError(TypeMismatch, MissingValue, AtKey)
  , decodeAeson
  , decodeJsonString
  , getField
  , getFieldOptional
  , getFieldOptional'
  , isNull
  , parseJsonStringToAeson
  , stringifyAeson
  , unpackFinite
  , (.:)
  , (.:!)
  )
import Affjax (Error, Response, URL, defaultRequest, printError, request) as Affjax
import Affjax.RequestBody (RequestBody, arrayView, string) as Affjax
import Affjax.RequestHeader (RequestHeader(ContentType, RequestHeader)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Contract.Prelude (mconcat)
import Contract.RewardAddress
  ( rewardAddressToBech32
  , stakePubKeyHashRewardAddress
  , stakeValidatorHashRewardAddress
  )
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Control.Monad.Logger.Class (log)
import Control.Monad.Logger.Trans (LoggerT(LoggerT), runLoggerT)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Parallel (parTraverse)
import Ctl.Internal.Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptAll
      , ScriptAny
      , ScriptNOfK
      , ScriptPubkey
      , TimelockExpiry
      , TimelockStart
      )
  )
import Ctl.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  )
import Ctl.Internal.Cardano.Types.Transaction
  ( Costmdls(Costmdls)
  , PoolPubKeyHash
  , Transaction
  , TransactionOutput(TransactionOutput)
  , UtxoMap
  , poolPubKeyHashToBech32
  )
import Ctl.Internal.Cardano.Types.Value (Coin(Coin), Value)
import Ctl.Internal.Cardano.Types.Value
  ( lovelaceValueOf
  , mkSingletonNonAdaAsset
  , mkValue
  ) as Value
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.Contract.QueryHandle.Error
  ( GetTxMetadataError
      ( GetTxMetadataTxNotFoundError
      , GetTxMetadataClientError
      , GetTxMetadataMetadataEmptyOrMissingError
      )
  )
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.PlutusData (deserializeData)
import Ctl.Internal.Deserialization.Transaction
  ( convertGeneralTransactionMetadata
  )
import Ctl.Internal.Plutus.Types.CurrencySymbol
  ( CurrencySymbol
  , adaSymbol
  , getCurrencySymbol
  )
import Ctl.Internal.QueryM.Ogmios (TxEvaluationR)
import Ctl.Internal.QueryM.Pools (DelegationsAndRewards)
import Ctl.Internal.Serialization as Serialization
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId
  , addressBech32
  , addressFromBech32
  )
import Ctl.Internal.Serialization.Hash
  ( ScriptHash
  , ed25519KeyHashFromBytes
  , scriptHashToBytes
  )
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
  , decodePlutusAssetClass
  )
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex)
import Ctl.Internal.Types.CborBytes (CborBytes, cborBytesToHex)
import Ctl.Internal.Types.Chain (Tip(Tip, TipAtGenesis))
import Ctl.Internal.Types.Datum (DataHash(DataHash), Datum)
import Ctl.Internal.Types.Epoch (Epoch(Epoch))
import Ctl.Internal.Types.EraSummaries
  ( EraSummaries
  , EraSummary
  , EraSummaryParameters
  )
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatum, OutputDatumHash)
  )
import Ctl.Internal.Types.ProtocolParameters
  ( CoinsPerUtxoUnit(CoinsPerUtxoWord, CoinsPerUtxoByte)
  , CostModelV1
  , CostModelV2
  , ProtocolParameters(ProtocolParameters)
  , convertPlutusV1CostModel
  , convertPlutusV2CostModel
  )
import Ctl.Internal.Types.PubKeyHash (StakePubKeyHash)
import Ctl.Internal.Types.Rational (Rational, reduce)
import Ctl.Internal.Types.RawBytes (rawBytesToHex)
import Ctl.Internal.Types.Scripts
  ( Language(PlutusV2, PlutusV1)
  , StakeValidatorHash
  , plutusV1Script
  , plutusV2Script
  )
import Ctl.Internal.Types.SystemStart (SystemStart(SystemStart))
import Ctl.Internal.Types.TokenName (TokenName, adaToken, getTokenName)
import Ctl.Internal.Types.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  )
import Ctl.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  )
import Data.Array (find, length, nub) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt (fromString, toNumber) as BigInt
import Data.BigNumber (BigNumber, toFraction)
import Data.BigNumber as BigNumber
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(Left, Right), either, note)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET, POST))
import Data.JSDate (JSDate, now)
import Data.Log.Level (LogLevel(Trace))
import Data.Log.Message (Message)
import Data.Map (empty, fromFoldable, isEmpty, unions) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.MediaType (MediaType(MediaType))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number (infinity)
import Data.Show.Generic (genericShow)
import Data.String (splitAt) as String
import Data.Time.Duration (Seconds(Seconds), convertDuration)
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(Tuple), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (Object)
import Safe.Coerce (coerce)

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
  -- /utils/txs/evaluate
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
  -- /assets/{asset}/addresses?page={page}&count={count}
  | AssetAddresses CurrencySymbol TokenName Int Int
  -- /assets/policy/{policy_id}?page={page}&count={count}
  | AssetsOfPolicy CurrencySymbol Int Int
  -- /addresses/{address}/utxos/{asset}?page={page}&count={count}
  | AssetUtxosAtAddress Address CurrencySymbol TokenName Int Int
  -- /assets/{asset}/transactions
  | AssetTransactions CurrencySymbol TokenName Int Int

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
    DatumCbor (DataHash hashBytes) ->
      "/scripts/datum/" <> byteArrayToHex hashBytes <> "/cbor"
    EraSummaries ->
      "/network/eras"
    EvaluateTransaction ->
      "/utils/txs/evaluate"
    LatestBlock ->
      "/blocks/latest"
    LatestEpoch ->
      "/epochs/latest"
    LatestProtocolParameters ->
      "/epochs/latest/parameters"
    NativeScriptByHash scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash) <> "/json"
    PlutusScriptCborByHash scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash) <> "/cbor"
    ScriptInfo scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash)
    SubmitTransaction ->
      "/tx/submit"
    Transaction txHash ->
      "/txs/" <> byteArrayToHex (unwrap txHash)
    TransactionMetadata txHash ->
      "/txs/" <> byteArrayToHex (unwrap txHash) <> "/metadata/cbor"
    UtxosAtAddress address page count ->
      "/addresses/" <> addressBech32 address <> "/utxos?page=" <> show page
        <> ("&count=" <> show count)
    UtxosOfTransaction txHash ->
      "/txs/" <> byteArrayToHex (unwrap txHash) <> "/utxos"
    PoolIds page count ->
      "/pools?page=" <> show page <> "&count=" <> show count <> "&order=asc"
    PoolParameters poolPubKeyHash ->
      "/pool/" <> poolPubKeyHashToBech32 poolPubKeyHash
    DelegationsAndRewards credential ->
      "/accounts/" <> blockfrostStakeCredentialToBech32 credential
    AssetAddresses symbol name page count ->
      let
        encodedCurrencySymbol = byteArrayToHex $ getCurrencySymbol symbol
        encodedTokenName = byteArrayToHex $ getTokenName name
      in
        "/assets/" <> encodedCurrencySymbol <> encodedTokenName <> "/addresses"
          <> "/utxos?page="
          <> show page
          <> ("&count=" <> show count)
          <> "&order=asc"
    AssetsOfPolicy symbol page count ->
      let
        encodedCurrencySymbol = byteArrayToHex $ getCurrencySymbol symbol
      in
        "/assets/policy/" <> encodedCurrencySymbol <> "?page" <> show page
          <> "&count"
          <> show count
          <> "&order=asc"
    AssetUtxosAtAddress address symbol name page count ->
      let
        encodedAddress = addressBech32 address
        encodedCurrencySymbol = byteArrayToHex $ getCurrencySymbol symbol
        encodedTokenName = byteArrayToHex $ getTokenName name
      in
        "/addresses/" <> encodedAddress <> "/utxos/" <> encodedCurrencySymbol
          <> encodedTokenName
          <> "?page="
          <> show page
          <> ("&count=" <> show count)
          <> "&order=asc"
    AssetTransactions symbol name page count ->
      let
        encodedCurrencySymbol = byteArrayToHex $ getCurrencySymbol symbol
        encodedTokenName = byteArrayToHex $ getTokenName name
      in
        "/assets/" <> encodedCurrencySymbol <> encodedTokenName
          <> "/transactions"
          <> "?page="
          <> show page
          <> ("&count=" <> show count)
          <> "&order=asc"

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
    case Array.length (unwrap utxos) /= maxNumResultsOnPage of
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

utxosInTransaction
  :: TransactionHash
  -> BlockfrostServiceM (Either ClientError UtxoMap)
utxosInTransaction txHash = runExceptT $ do
  (blockfrostUtxoMap :: BlockfrostUtxosOfTransaction) <- ExceptT $
    entriesOnPage (\_ _ -> UtxosOfTransaction txHash) 1
  ExceptT $ resolveBlockfrostUtxosAtAddress (coerce blockfrostUtxoMap)

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
-- Get utxos of a specific asset class
--------------------------------------------------------------------------------

utxosWithAssetClass
  :: CurrencySymbol
  -> TokenName
  -> BlockfrostServiceM (Either ClientError UtxoMap)
utxosWithAssetClass symbol name = runExceptT $ do
  addresses :: BlockfrostAssetAddresses <- ExceptT
    (entriesOnPage addressesOnPage 1)
  utxosOfAssetClassAtAddresses :: Array BlockfrostUtxosAtAddress <-
    traverse (ExceptT <<< utxosOfAssetClassAtAddress) $ unwrap addresses
  ExceptT $ resolveBlockfrostUtxosAtAddress
    (mconcat utxosOfAssetClassAtAddresses)
  where
  addressesOnPage :: Int -> Int -> BlockfrostEndpoint
  addressesOnPage = AssetAddresses symbol name

  assetUtxosAtAddressOnPage :: Address -> Int -> Int -> BlockfrostEndpoint
  assetUtxosAtAddressOnPage address =
    AssetUtxosAtAddress address symbol name

  utxosOfAssetClassAtAddress
    :: Address
    -> BlockfrostServiceM (Either ClientError BlockfrostUtxosAtAddress)
  utxosOfAssetClassAtAddress address = entriesOnPage
    (assetUtxosAtAddressOnPage address)
    1

entriesOnPage
  :: forall (result :: Type) (a :: Type)
   . Monoid result
  => DecodeAeson result
  => Newtype result (Array a)
  => (Int -> Int -> BlockfrostEndpoint)
  -> Int
  -> BlockfrostServiceM (Either ClientError result)
entriesOnPage paginatedEndpoint page = runExceptT $ do
  let
    -- Maximum number of results per page supported by Blockfrost:
    maxNumResultsOnPage :: Int
    maxNumResultsOnPage = 100

  utxos <- ExceptT $
    blockfrostGetRequest (paginatedEndpoint page maxNumResultsOnPage)
      <#> handle404AsMempty <<< handleBlockfrostResponse
  if Array.length (unwrap utxos) /= maxNumResultsOnPage then pure utxos
  else append utxos <$> ExceptT (entriesOnPage paginatedEndpoint $ page + 1)

--------------------------------------------------------------------------------
-- Get utxos of a specific currency symbol
--------------------------------------------------------------------------------

utxosWithCurrencySymbol
  :: CurrencySymbol
  -> BlockfrostServiceM (Either ClientError UtxoMap)
utxosWithCurrencySymbol symbol = runExceptT $ do
  assets :: BlockfrostAssetsWithCurrencySymbol <- ExceptT
    (entriesOnPage assetsOnPage 1)
  utxos <-
    traverse
      (ExceptT <<< uncurry utxosWithAssetClass)
      $ unwrap assets
  pure $ Map.unions utxos
  where
  assetsOnPage :: Int -> Int -> BlockfrostEndpoint
  assetsOnPage = AssetsOfPolicy symbol

allOutputsWithCurrencySymbol
  :: CurrencySymbol
  -> BlockfrostServiceM (Either ClientError UtxoMap)
allOutputsWithCurrencySymbol symbol = runExceptT $ do
  assets :: BlockfrostAssetsWithCurrencySymbol <- ExceptT
    (entriesOnPage assetsOnPage 1)
  transactions :: BlockfrostAssetTransactions <-
    mconcat
      <$> traverse
        (ExceptT <<< uncurry assetTransactions)
        (unwrap assets)
  utxos <-
    traverse
      (ExceptT <<< utxosInTransaction)
      $ Array.nub
      $ unwrap transactions
  pure $ Map.unions utxos
  where
  assetsOnPage :: Int -> Int -> BlockfrostEndpoint
  assetsOnPage = AssetsOfPolicy symbol

  assetTransactions
    :: CurrencySymbol
    -> TokenName
    -> BlockfrostServiceM (Either ClientError BlockfrostAssetTransactions)
  assetTransactions _ name = entriesOnPage
    (AssetTransactions symbol name)
    1

--------------------------------------------------------------------------------
-- Get datum by hash
--------------------------------------------------------------------------------

getDatumByHash
  :: DataHash -> BlockfrostServiceM (Either ClientError (Maybe Datum))
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
      PlutusScriptRef <<< plutusV1Script <$>
        MaybeT (ExceptT getPlutusScriptCborByHash)
    PlutusV2Script ->
      PlutusScriptRef <<< plutusV2Script <$>
        MaybeT (ExceptT getPlutusScriptCborByHash)
  where
  getNativeScriptByHash
    :: BlockfrostServiceM (Either ClientError (Maybe NativeScript))
  getNativeScriptByHash =
    blockfrostGetRequest (NativeScriptByHash scriptHash) <#> \response ->
      map unwrapBlockfrostNativeScript <$>
        handle404AsNothing (handleBlockfrostResponse response)

  getPlutusScriptCborByHash
    :: BlockfrostServiceM (Either ClientError (Maybe ByteArray))
  getPlutusScriptCborByHash =
    blockfrostGetRequest (PlutusScriptCborByHash scriptHash) <#> \response ->
      handle404AsNothing
        (unwrapBlockfrostCbor <$> handleBlockfrostResponse response)

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
  cslTx <- liftEffect $ Serialization.convertTransaction tx
  handleBlockfrostResponse <$> request (Serialization.toBytes cslTx)
  where
  request
    :: CborBytes
    -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
  request cbor =
    blockfrostPostRequest SubmitTransaction (MediaType "application/cbor")
      (Just $ Affjax.arrayView $ unwrap $ unwrap cbor)

evaluateTx :: Transaction -> BlockfrostServiceM TxEvaluationR
evaluateTx tx = do
  cslTx <- liftEffect $ Serialization.convertTransaction tx
  resp <- handleBlockfrostResponse <$> request (Serialization.toBytes cslTx)
  case unwrapBlockfrostEvaluateTx <$> resp of
    Left err -> throwError $ error $ show err
    Right (Left err) ->
      -- Replicate the error of QueryM's fault handler
      throwError $ error $ "Server responded with `fault`: " <> stringifyAeson
        err
    Right (Right eval) -> pure eval
  where
  -- Hex encoded, not binary like submission
  request
    :: CborBytes
    -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
  request cbor =
    blockfrostPostRequest EvaluateTransaction (MediaType "application/cbor")
      (Just $ Affjax.string $ cborBytesToHex cbor)

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
-- Get transaction metadata
--------------------------------------------------------------------------------

getTxMetadata
  :: TransactionHash
  -> BlockfrostServiceM (Either GetTxMetadataError GeneralTransactionMetadata)
getTxMetadata txHash = do
  response <- blockfrostGetRequest (TransactionMetadata txHash)
  pure case unwrapBlockfrostMetadata <$> handleBlockfrostResponse response of
    Left (ClientHttpResponseError (Affjax.StatusCode 404) _) ->
      Left GetTxMetadataTxNotFoundError
    Left e ->
      Left (GetTxMetadataClientError e)
    Right metadata
      | Map.isEmpty (unwrap metadata) ->
          Left GetTxMetadataMetadataEmptyOrMissingError
      | otherwise -> Right metadata

--------------------------------------------------------------------------------
-- Get current epoch information
--------------------------------------------------------------------------------

getCurrentEpoch :: BlockfrostServiceM (Either ClientError BigInt)
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
      { result } :: { result :: TxEvaluationR } <- decodeAeson aeson
      pure $ Right result

    failure :: Either JsonDecodeError (Either Aeson TxEvaluationR)
    failure = pure $ Left aeson

unwrapBlockfrostEvaluateTx :: BlockfrostEvaluateTx -> Either Aeson TxEvaluationR
unwrapBlockfrostEvaluateTx (BlockfrostEvaluateTx ei) = ei

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
-- BlockfrostAssetAddresses
--------------------------------------------------------------------------------

newtype BlockfrostAssetAddresses = BlockfrostAssetAddresses (Array Address)

derive instance Generic BlockfrostAssetAddresses _
derive instance Newtype BlockfrostAssetAddresses _
derive newtype instance Semigroup BlockfrostAssetAddresses
derive newtype instance Monoid BlockfrostAssetAddresses

instance Show BlockfrostAssetAddresses where
  show = genericShow

instance DecodeAeson BlockfrostAssetAddresses where
  decodeAeson = aesonArray (map wrap <<< traverse decodeAddressEntry)
    where
    decodeAddressEntry :: Aeson -> Either JsonDecodeError Address
    decodeAddressEntry = aesonObject $ \obj -> do
      bech32Address <- getField obj "address"
      note (TypeMismatch "Expected bech32 encoded address")
        (addressFromBech32 bech32Address)

--------------------------------------------------------------------------------
-- BlockfrostAssetTransactions
--------------------------------------------------------------------------------

newtype BlockfrostAssetTransactions = BlockfrostAssetTransactions
  (Array TransactionHash)

derive instance Generic BlockfrostAssetTransactions _
derive instance Newtype BlockfrostAssetTransactions _
derive newtype instance Semigroup BlockfrostAssetTransactions
derive newtype instance Monoid BlockfrostAssetTransactions

instance Show BlockfrostAssetTransactions where
  show = genericShow

instance DecodeAeson BlockfrostAssetTransactions where
  decodeAeson = aesonArray (map wrap <<< traverse decodeTransactionEntry)
    where
    decodeTransactionEntry :: Aeson -> Either JsonDecodeError TransactionHash
    decodeTransactionEntry = aesonObject $ \obj -> getField obj "tx_hash"

--------------------------------------------------------------------------------
-- BlockfrostAssetsWithCurrencySymbol
--------------------------------------------------------------------------------

newtype BlockfrostAssetsWithCurrencySymbol = BlockfrostAssetsWithCurrencySymbol
  (Array (CurrencySymbol /\ TokenName))

derive instance Generic BlockfrostAssetsWithCurrencySymbol _
derive instance Newtype BlockfrostAssetsWithCurrencySymbol _
derive newtype instance Semigroup BlockfrostAssetsWithCurrencySymbol
derive newtype instance Monoid BlockfrostAssetsWithCurrencySymbol

instance Show BlockfrostAssetsWithCurrencySymbol where
  show = genericShow

instance DecodeAeson BlockfrostAssetsWithCurrencySymbol where
  decodeAeson = aesonArray (map wrap <<< traverse decodeAddressEntry)
    where
    decodeAddressEntry
      :: Aeson -> Either JsonDecodeError (CurrencySymbol /\ TokenName)
    decodeAddressEntry = aesonObject $ \obj -> do
      getField obj "asset" >>= case _ of
        "lovelace" -> pure $ adaSymbol /\ adaToken
        assetString -> do
          let { before: symbol, after: name } = String.splitAt 56 assetString
          decodePlutusAssetClass assetString symbol name

--------------------------------------------------------------------------------
-- BlockfrostTransactionOutput
--------------------------------------------------------------------------------

newtype BlockfrostTransactionOutput = BlockfrostTransactionOutput
  { address :: Address
  , amount :: Value
  , datum :: OutputDatum
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
          (addressFromBech32 address)

    decodeValue :: Object Aeson -> Either JsonDecodeError Value
    decodeValue =
      flip getField "amount" >=> aesonArray (map fold <<< traverse decodeAsset)
      where
      decodeAsset :: Aeson -> Either JsonDecodeError Value
      decodeAsset = aesonObject \obj -> do
        quantity <-
          getField obj "quantity" >>=
            BigInt.fromString >>>
              note (TypeMismatch "Expected string repr of BigInt")
        getField obj "unit" >>= case _ of
          "lovelace" -> pure $ Value.lovelaceValueOf quantity
          assetString -> do
            let { before: csStr, after: tnStr } = String.splitAt 56 assetString
            decodeAssetClass assetString csStr tnStr <#> \(cs /\ tn) ->
              Value.mkValue mempty $ Value.mkSingletonNonAdaAsset cs tn quantity

    decodeOutputDatum :: Object Aeson -> Either JsonDecodeError OutputDatum
    decodeOutputDatum obj =
      getFieldOptional' obj "inline_datum" >>= case _ of
        Just datum ->
          note (TypeMismatch "Expected CBOR encoded inline datum")
            (OutputDatum <$> deserializeData datum)
        Nothing ->
          maybe NoOutputDatum OutputDatumHash
            <$> getFieldOptional' obj "data_hash"

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
              (note (TypeMismatch "Ed25519KeyHash") <<< ed25519KeyHashFromBytes)
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

newtype BlockfrostDatum = BlockfrostDatum (Maybe Datum)

derive instance Generic BlockfrostDatum _
derive instance Newtype BlockfrostDatum _

instance Show BlockfrostDatum where
  show = genericShow

unwrapBlockfrostDatum :: BlockfrostDatum -> Maybe Datum
unwrapBlockfrostDatum = unwrap

instance DecodeAeson BlockfrostDatum where
  decodeAeson aeson
    | isNull aeson = pure $ BlockfrostDatum Nothing
    | otherwise = do
        cbor <- aesonObject (flip getFieldOptional "cbor") aeson
        pure $ BlockfrostDatum $ deserializeData =<< cbor

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
        map (unwrap <<< convertGeneralTransactionMetadata) <$> flip note
          (fromBytes metadata) $
          TypeMismatch "Hexadecimal encoded Metadata"

      pure $ BlockfrostMetadata $ GeneralTransactionMetadata $ Map.unions
        metadatas'

unwrapBlockfrostMetadata :: BlockfrostMetadata -> GeneralTransactionMetadata
unwrapBlockfrostMetadata (BlockfrostMetadata metadata) = metadata

--------------------------------------------------------------------------------
-- BlockfrostCurrentEpoch
--------------------------------------------------------------------------------

newtype BlockfrostCurrentEpoch = BlockfrostCurrentEpoch BigInt

derive instance Generic BlockfrostCurrentEpoch _
derive instance Newtype BlockfrostCurrentEpoch _

instance Show BlockfrostCurrentEpoch where
  show = genericShow

instance DecodeAeson BlockfrostCurrentEpoch where
  decodeAeson a = decodeAeson a <#>
    \({ epoch } :: { epoch :: BigInt }) -> wrap epoch

unwrapBlockfrostCurrentEpoch :: BlockfrostCurrentEpoch -> BigInt
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
    rewardAddressToBech32 $ stakePubKeyHashRewardAddress networkId
      stakePubKeyHash
  BlockfrostStakeCredential networkId (Right stakeValidatorHash) ->
    rewardAddressToBech32 $ stakeValidatorHashRewardAddress networkId
      stakeValidatorHash

--------------------------------------------------------------------------------
-- BlockfrostProtocolParameters
--------------------------------------------------------------------------------

-- | `Stringed a` decodes an `a` who was encoded as a `String`
newtype Stringed a = Stringed a

derive instance Newtype (Stringed a) _

instance DecodeAeson a => DecodeAeson (Stringed a) where
  decodeAeson = decodeAeson >=> decodeJsonString >=> Stringed >>> pure

type BlockfrostProtocolParametersRaw =
  { "min_fee_a" :: UInt
  , "min_fee_b" :: UInt
  , "max_block_size" :: UInt
  , "max_tx_size" :: UInt
  , "max_block_header_size" :: UInt
  , "key_deposit" :: Stringed BigInt
  , "pool_deposit" :: Stringed BigInt
  , "e_max" :: BigInt
  , "n_opt" :: UInt
  , "a0" :: Finite BigNumber
  , "rho" :: Finite BigNumber
  , "tau" :: Finite BigNumber
  , "protocol_major_ver" :: UInt
  , "protocol_minor_ver" :: UInt
  , "min_pool_cost" :: Stringed BigInt
  , "cost_models" ::
      { "PlutusV1" :: { | CostModelV1 }
      , "PlutusV2" :: { | CostModelV2 }
      }
  , "price_mem" :: Finite BigNumber
  , "price_step" :: Finite BigNumber
  , "max_tx_ex_mem" :: Stringed BigInt
  , "max_tx_ex_steps" :: Stringed BigInt
  , "max_block_ex_mem" :: Stringed BigInt
  , "max_block_ex_steps" :: Stringed BigInt
  , "max_val_size" :: Stringed UInt
  , "collateral_percent" :: UInt
  , "max_collateral_inputs" :: UInt
  , "coins_per_utxo_size" :: Maybe (Stringed BigInt)
  , "coins_per_utxo_word" :: Maybe (Stringed BigInt)
  }

toFraction' :: Finite BigNumber -> String /\ String
toFraction' bn =
  (BigNumber.toString numerator /\ BigNumber.toString denominator)
  where
  (numerator /\ denominator) = toFraction (unpackFinite bn)
    (BigNumber.fromNumber infinity)

bigNumberToRational :: Finite BigNumber -> Either JsonDecodeError Rational
bigNumberToRational bn = note (TypeMismatch "Rational") do
  numerator <- BigInt.fromString numerator'
  denominator <- BigInt.fromString denominator'
  reduce numerator denominator
  where
  (numerator' /\ denominator') = toFraction' bn

bigNumberToPrice
  :: Finite BigNumber
  -> Either JsonDecodeError { numerator :: BigNum, denominator :: BigNum }
bigNumberToPrice bn = note (TypeMismatch "Rational") do
  numerator <- BigNum.fromString numerator'
  denominator <- BigNum.fromString denominator'
  pure { numerator, denominator }
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
    let prices = { memPrice, stepPrice }

    coinsPerUtxoUnit <-
      maybe
        (Left $ AtKey "coinsPerUtxoByte or coinsPerUtxoWord" $ MissingValue)
        pure
        $ (CoinsPerUtxoByte <<< Coin <<< unwrap <$> raw.coins_per_utxo_size) <|>
            (CoinsPerUtxoWord <<< Coin <<< unwrap <$> raw.coins_per_utxo_word)

    pure $ BlockfrostProtocolParameters $ ProtocolParameters
      { protocolVersion: raw.protocol_major_ver /\ raw.protocol_minor_ver
      -- The following two parameters were removed from Babbage
      , decentralization: zero
      , extraPraosEntropy: Nothing
      , maxBlockHeaderSize: raw.max_block_header_size
      , maxBlockBodySize: raw.max_block_size
      , maxTxSize: raw.max_tx_size
      , txFeeFixed: raw.min_fee_b
      , txFeePerByte: raw.min_fee_a
      , stakeAddressDeposit: Coin $ unwrap raw.key_deposit
      , stakePoolDeposit: Coin $ unwrap raw.pool_deposit
      , minPoolCost: Coin $ unwrap raw.min_pool_cost
      , poolRetireMaxEpoch: Epoch raw.e_max
      , stakePoolTargetNum: raw.n_opt
      , poolPledgeInfluence
      , monetaryExpansion
      , treasuryCut
      , coinsPerUtxoUnit: coinsPerUtxoUnit
      , costModels: Costmdls $ Map.fromFoldable
          [ PlutusV1 /\ convertPlutusV1CostModel raw.cost_models."PlutusV1"
          , PlutusV2 /\ convertPlutusV2CostModel raw.cost_models."PlutusV2"
          ]
      , prices
      , maxTxExUnits:
          { mem: unwrap raw.max_tx_ex_mem
          , steps: unwrap raw.max_tx_ex_steps
          }
      , maxBlockExUnits:
          { mem: unwrap raw.max_block_ex_mem
          , steps: unwrap raw.max_block_ex_steps
          }
      , maxValueSize: unwrap raw.max_val_size
      , collateralPercent: raw.collateral_percent
      , maxCollateralInputs: raw.max_collateral_inputs
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
        note (TypeMismatch "BigInt") $ map Coin $ BigInt.fromString
          withdrawable_amount_str
    pure $ BlockfrostRewards
      { pool_id
      , withdrawable_amount
      }
