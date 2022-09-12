module Api (
  app,
  applyArgs,
  apiDocs,
) where

import Api.Handlers qualified as Handlers
import Data.Proxy (Proxy (Proxy))
import Network.Wai.Middleware.Cors qualified as Cors
import Servant (
  Application,
  JSON,
  Post,
  ReqBody,
  Server,
  serve,
  type (:>),
 )
import Servant.Client (ClientM, client)
import Servant.Docs qualified as Docs
import Types (
  AppliedScript,
  ApplyArgsRequest,
 )

type Api =
  -- Since @Script@ and @Data@ have @From/ToJSON@ instances, we can just
  -- accept them in the body of a POST request
  "apply-args"
    :> ReqBody '[JSON] ApplyArgsRequest
    :> Post '[JSON] AppliedScript

app :: Application
app = Cors.cors (const $ Just policy) $ serve api server
  where
    policy :: Cors.CorsResourcePolicy
    policy =
      Cors.simpleCorsResourcePolicy
        { Cors.corsRequestHeaders = ["Content-Type"]
        , Cors.corsMethods = ["OPTIONS", "GET", "POST"]
        }

api :: Proxy Api
api = Proxy

server :: Server Api
server = pure . Handlers.applyArgs

apiDocs :: Docs.API
apiDocs = Docs.docs api

applyArgs :: ApplyArgsRequest -> ClientM AppliedScript
applyArgs = client api
