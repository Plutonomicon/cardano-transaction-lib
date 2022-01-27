module Main (main) where

import Api (app)
import Data.Function ((&))
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setLogger,
  setPort,
 )
import Network.Wai.Logger (withStdoutLogger)
import System.Exit (die)
import Types (newEnvIO)

main :: IO ()
main = withStdoutLogger $ \logger ->
  runSettings (mkSettings logger)
    . app
    =<< either die pure
    =<< newEnvIO
  where
    mkSettings logger = defaultSettings & setPort 8081 & setLogger logger
