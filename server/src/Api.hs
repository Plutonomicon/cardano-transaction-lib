module Api (
  app,
  estimateTxFees,
  applyArgs,
  finalizeTx,
  hashScript,
  apiDocs
) where

import Api.Handlers qualified as Handlers
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
  CardanoBrowserServerError (FeeEstimate, FinalizeTx),
  Cbor,
  Env,
  Fee,
  FeeEstimateError (FEInvalidCbor, FEInvalidHex),
  FinalizeTxError (FTInvalidCbor, FTInvalidHex),
  HashScriptRequest,
  HashedScript,
  FinalizeRequest(..),
  FinalizedTransaction(..)
 )
import Utils (lbshow)


type Api =
  "fees" :> QueryParam' '[Required] "tx" Cbor :> Get '[JSON] Fee
    -- Since @Script@ and @Data@ have @From/ToJSON@ instances, we can just
    -- accept them in the body of a POST request
    :<|> "apply-args"
      :> ReqBody '[JSON] ApplyArgsRequest
      :> Post '[JSON] AppliedScript
    :<|> "hash-script"
      :> ReqBody '[JSON] HashScriptRequest
      :> Post '[JSON] HashedScript
    :<|> "finalize"
      :> ReqBody '[JSON] FinalizeRequest
      :> Post '[JSON] FinalizedTransaction

app :: Env -> Application
app = Cors.cors (const $ Just policy) . serve api . appServer
  where
    policy :: Cors.CorsResourcePolicy
    policy = Cors.simpleCorsResourcePolicy
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
          Handler (Either CardanoBrowserServerError a)
        tryServer =
          liftIO
            . try @_ @CardanoBrowserServerError
            . flip runReaderT env

        handleError ::
          CardanoBrowserServerError ->
          Handler a
        handleError (FeeEstimate fe) = case fe of
          FEInvalidCbor ic -> throwError err400 {errBody = lbshow ic}
          FEInvalidHex ih -> throwError err400 {errBody = LC8.pack ih}
        handleError (FinalizeTx fe) = case fe of
          FTInvalidCbor ic -> throwError err400 {errBody = lbshow ic}
          FTInvalidHex ih -> throwError err400 {errBody = LC8.pack ih}

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server =
  Handlers.estimateTxFees
    :<|> Handlers.applyArgs
    :<|> Handlers.hashScript
    :<|> Handlers.finalizeTx

apiDocs :: Docs.API
apiDocs = Docs.docs api

estimateTxFees :: Cbor -> ClientM Fee
applyArgs :: ApplyArgsRequest -> ClientM AppliedScript
hashScript :: HashScriptRequest -> ClientM HashedScript
finalizeTx :: FinalizeRequest -> ClientM FinalizedTransaction
estimateTxFees :<|> applyArgs :<|> hashScript :<|> finalizeTx = client api
