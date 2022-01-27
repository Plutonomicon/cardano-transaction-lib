module Api (app, getTransactionFeeEstimate) where

import Api.Fees (estimateTxFees)
import Control.Monad.Catch (try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Lazy.Char8 qualified as LC8
import Servant (
  Application,
  Get,
  Handler,
  HasServer (ServerT),
  JSON,
  Proxy (..),
  QueryParam',
  Required,
  Server,
  ServerError (errBody),
  err400,
  hoistServer,
  serve,
  type (:>),
 )
import Servant.Client (ClientM, client)
import Types (
  AppM (..),
  CardanoBrowserServerError (..),
  Cbor,
  Env,
  Fee,
  FeeEstimateError (InvalidCbor, InvalidHex),
 )
import Utils (lbshow)

type Api = "fees" :> QueryParam' '[Required] "tx" Cbor :> Get '[JSON] Fee

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
      InvalidCbor ic -> throwError err400 {errBody = lbshow ic}
      InvalidHex ih -> throwError err400 {errBody = LC8.pack ih}

api :: Proxy Api
api = Proxy @Api

server :: ServerT Api AppM
server = estimateTxFees

getTransactionFeeEstimate :: Cbor -> ClientM Fee
getTransactionFeeEstimate = client api
