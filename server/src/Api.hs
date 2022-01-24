module Api () where

import Servant
import Types

type Api = "fees" :> Capture "tx" Cbor :> Post '[JSON] FeeEstimate

api :: Proxy Api
api = Proxy

appServer :: Env -> Server Api
appServer env = hoistServer api (appHandler env) server
  where
    appHandler :: Env -> AppM a -> Handler a
    appHandler _ (AppM _) = undefined -- TODO

server :: ServerT Api AppM
server = undefined -- TODO
