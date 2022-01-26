module Main (main) where

import Api (app)
import Cardano.Api.Shelley qualified as C
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setLogger,
  setPort,
 )
import Network.Wai.Logger (withStdoutLogger)
import Paths_cardano_browser_tx_server (getDataFileName)
import System.Exit (die)
import Types (Env (..))

main :: IO ()
main = withStdoutLogger $ \logger -> do
  protocolParams <-
    either die pure
      =<< Aeson.eitherDecodeFileStrict @C.ProtocolParameters
      =<< getDataFileName "config/pparams.json"
  runSettings (mkSettings logger) . app $ Env {..}
  where
    mkSettings logger = defaultSettings & setPort 8081 & setLogger logger
