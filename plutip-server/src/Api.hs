module Api where

import Api.Handlers (
  startClusterHandler,
  stopClusterHandler,
 )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Kind (Type)
import Network.Wai.Middleware.Cors qualified as Cors
import Servant (
  Application,
  Handler,
  JSON,
  Post,
  Proxy (Proxy),
  ReqBody,
  Server,
  ServerT,
  hoistServer,
  serve,
  (:<|>) ((:<|>)),
  (:>),
 )
import Types (
  AppM (AppM),
  Env (Env),
  ServerOptions,
  StartClusterRequest,
  StartClusterResponse,
  StopClusterRequest,
  StopClusterResponse,
  options,
 )

type Api =
  "start"
    :> ReqBody '[JSON] StartClusterRequest
    :> Post '[JSON] StartClusterResponse
    :<|> "stop"
      :> ReqBody '[JSON] StopClusterRequest
      :> Post '[JSON] StopClusterResponse

app :: Env -> Application
app = Cors.cors (const $ Just policy) . serve api . appServer
  where
    policy :: Cors.CorsResourcePolicy
    policy =
      Cors.simpleCorsResourcePolicy
        { Cors.corsRequestHeaders = ["Content-Type"]
        , Cors.corsMethods = ["OPTIONS", "GET", "POST"]
        }

api :: Proxy Api
api = Proxy

server :: ServerOptions -> ServerT Api AppM
server serverOptions =
  startClusterHandler serverOptions
    :<|> stopClusterHandler

appServer :: Env -> Server Api
appServer env@Env {options} =
  hoistServer api appHandler (server options)
  where
    appHandler :: forall (a :: Type). AppM a -> Handler a
    appHandler (AppM x) = liftIO $ runReaderT x env
