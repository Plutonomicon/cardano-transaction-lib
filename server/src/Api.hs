module Api (app) where

import Api.Fees
import Control.Monad.Catch (try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Servant (
  Capture,
  Handler,
  HasServer (ServerT),
  JSON,
  Post,
  Proxy (..),
  Server,
  ServerError (errBody),
  err400,
  hoistServer,
  type (:>), Application, serve
 )
import Types
import Utils

type Api = "fees" :> Capture "tx" Cbor :> Post '[JSON] Fee

app :: Env -> Application
app = serve api . appServer

appServer :: Env -> Server Api
appServer env = hoistServer api appHandler server
  where
    appHandler :: AppM a -> Handler a
    appHandler (AppM x) = tryServer >>= either handleError pure
      where
        tryServer =
          liftIO
            . try @_ @CardanoBrowserServerError
            $ runReaderT x env

    handleError :: CardanoBrowserServerError -> Handler a
    handleError (FeeEstimate fe) = case fe of
      DecoderError de -> throwError err400 {errBody = lbshow de}

api :: Proxy Api
api = Proxy @Api

server :: ServerT Api AppM
server = estimateTxFees
