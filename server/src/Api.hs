module Api (
  app,
  estimateTxFees,
  applyArgs,
  finalizeTx,
  evalTxExecutionUnits,
  apiDocs,
) where

import Api.Handlers qualified as Handlers
import Cardano.Api qualified as C (displayError)
import Control.Monad.Catch (try, catchAll)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Network.Wai.Middleware.Cors qualified as Cors
import Servant (
  Application,
  Handler,
  HasServer (ServerT),
  JSON,
  Post,
  ReqBody,
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
  CardanoError (
    AcquireFailure,
    EraMismatchError,
    ScriptExecutionError,
    TxValidityIntervalError
  ),
  CborDecodeError (InvalidCbor, InvalidHex, OtherDecodeError),
  CtlServerError (CardanoError, CborDecode, ErrorCall),
  Env,
  EvalExUnitsRequest,
  ExecutionUnitsMap,
  Fee,
  FeesRequest,
  FinalizeRequest,
  FinalizedTransaction,
 )
import Utils (lbshow)

type Api =
  "fees"
    :> ReqBody '[JSON] FeesRequest
    :> Post '[JSON] Fee
    -- Since @Script@ and @Data@ have @From/ToJSON@ instances, we can just
    -- accept them in the body of a POST request
    :<|> "apply-args"
      :> ReqBody '[JSON] ApplyArgsRequest
      :> Post '[JSON] AppliedScript
    :<|> "eval-ex-units"
      :> ReqBody '[JSON] EvalExUnitsRequest
      :> Post '[JSON] ExecutionUnitsMap
    :<|> "finalize"
      :> ReqBody '[JSON] FinalizeRequest
      :> Post '[JSON] FinalizedTransaction

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
        tryServer ra = catchAll
          (liftIO $ try @_ @CtlServerError $ runReaderT ra env)
          (pure . Left . ErrorCall)

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
        handleError (ErrorCall err) =
          throwError err400 {errBody = LC8.pack $ show err}

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server =
  Handlers.estimateTxFees
    :<|> Handlers.applyArgs
    :<|> Handlers.evalTxExecutionUnits
    :<|> Handlers.finalizeTx

apiDocs :: Docs.API
apiDocs = Docs.docs api

estimateTxFees :: FeesRequest -> ClientM Fee
applyArgs :: ApplyArgsRequest -> ClientM AppliedScript
evalTxExecutionUnits :: EvalExUnitsRequest -> ClientM ExecutionUnitsMap
finalizeTx :: FinalizeRequest -> ClientM FinalizedTransaction
estimateTxFees
  :<|> applyArgs
  :<|> evalTxExecutionUnits
  :<|> finalizeTx =
    client api
