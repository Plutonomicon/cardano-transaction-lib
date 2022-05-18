module Api (
  app,
  estimateTxFees,
  applyArgs,
  finalizeTx,
  hashData,
  hashScript,
  plutusHash,
  evalTxExecutionUnits,
  apiDocs,
) where

import Api.Handlers qualified as Handlers
import Cardano.Api qualified as C (displayError)
import Control.Monad.Catch (try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Network.Wai.Middleware.Cors qualified as Cors
import Servant (
  Application,
  Get,
  Handler,
  HasServer (ServerT),
  JSON,
  Post,
  QueryParam',
  ReqBody,
  Required,
  Server,
  ServerError (errBody),
  err400,
  hoistServer,
  serve,
  type (:<|>) ((:<|>)),
  type (:>),
 )
import Servant.Client (ClientM, client)
import Servant.Docs qualified as Docs
import Types (
  AppM (AppM),
  AppliedScript,
  ApplyArgsRequest,
  ByteStringHash,
  CardanoError (
    AcquireFailure,
    EraMismatchError,
    ScriptExecutionError,
    TxValidityIntervalError
  ),
  Cbor,
  CborDecodeError (InvalidCbor, InvalidHex, OtherDecodeError),
  CtlServerError (CardanoError, CborDecode),
  Env,
  ExecutionUnitsMap,
  Fee,
  FinalizeRequest,
  FinalizedTransaction,
  HashDataRequest,
  HashScriptRequest,
  HashBytesRequest,
  HashedData,
  HashedScript,
  WitnessCount,
 )
import Utils (lbshow)

type Api =
  "fees"
    :> QueryParam' '[Required] "count" WitnessCount
    :> QueryParam' '[Required] "tx" Cbor
    :> Get '[JSON] Fee
    -- Since @Script@ and @Data@ have @From/ToJSON@ instances, we can just
    -- accept them in the body of a POST request
    :<|> "apply-args"
      :> ReqBody '[JSON] ApplyArgsRequest
      :> Post '[JSON] AppliedScript
    :<|> "hash-script"
      :> ReqBody '[JSON] HashScriptRequest
      :> Post '[JSON] HashedScript
    -- Making this a POST request so we can just use the @From/ToJSON@
    -- instances instead of decoding in the handler
    :<|> "plutus-hash"
      :> ReqBody '[JSON] HashBytesRequest
      :> Post '[JSON] ByteStringHash
    :<|> "eval-ex-units"
      :> QueryParam' '[Required] "tx" Cbor
      :> Get '[JSON] ExecutionUnitsMap
    :<|> "finalize"
      :> ReqBody '[JSON] FinalizeRequest
      :> Post '[JSON] FinalizedTransaction
    :<|> "hash-data"
      :> ReqBody '[JSON] HashDataRequest
      :> Post '[JSON] HashedData

app :: Env -> Application
app = Cors.cors (const $ Just policy) . serve api . appServer
  where
    policy :: Cors.CorsResourcePolicy
    policy =
      Cors.simpleCorsResourcePolicy
        { Cors.corsRequestHeaders = ["Content-Type"]
        , Cors.corsMethods = ["OPTIONS", "GET", "POST"]
        }

appServer :: Env -> Server Api
appServer env = hoistServer api appHandler server
  where
    appHandler :: forall (a :: Type). AppM a -> Handler a
    appHandler (AppM x) = tryServer x >>= either handleError pure
      where
        tryServer ::
          ReaderT Env IO a ->
          Handler (Either CtlServerError a)
        tryServer =
          liftIO
            . try @_ @CtlServerError
            . flip runReaderT env

        handleError ::
          CtlServerError ->
          Handler a
        handleError (CardanoError ce) = case ce of
          AcquireFailure str ->
            throwError err400 {errBody = LC8.pack str}
          ScriptExecutionError err ->
            throwError err400 {errBody = LC8.pack (C.displayError err)}
          TxValidityIntervalError str ->
            throwError err400 {errBody = LC8.pack str}
          EraMismatchError ->
            throwError err400 {errBody = lbshow EraMismatchError}
        handleError (CborDecode de) = case de of
          InvalidCbor ic ->
            throwError err400 {errBody = lbshow ic}
          InvalidHex ih ->
            throwError err400 {errBody = LC8.pack ih}
          OtherDecodeError str ->
            throwError err400 {errBody = LC8.pack str}

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server =
  Handlers.estimateTxFees
    :<|> Handlers.applyArgs
    :<|> Handlers.hashScript
    :<|> Handlers.plutusHash
    :<|> Handlers.evalTxExecutionUnits
    :<|> Handlers.finalizeTx
    :<|> Handlers.hashData

apiDocs :: Docs.API
apiDocs = Docs.docs api

estimateTxFees :: WitnessCount -> Cbor -> ClientM Fee
applyArgs :: ApplyArgsRequest -> ClientM AppliedScript
hashScript :: HashScriptRequest -> ClientM HashedScript
plutusHash :: HashBytesRequest  -> ClientM ByteStringHash
evalTxExecutionUnits :: Cbor -> ClientM ExecutionUnitsMap
finalizeTx :: FinalizeRequest -> ClientM FinalizedTransaction
hashData :: HashDataRequest -> ClientM HashedData
estimateTxFees
  :<|> applyArgs
  :<|> hashScript
  :<|> plutusHash
  :<|> evalTxExecutionUnits
  :<|> finalizeTx
  :<|> hashData =
    client api
