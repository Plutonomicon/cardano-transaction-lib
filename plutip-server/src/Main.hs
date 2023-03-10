module Main (main) where

import Api (app)
import Control.Applicative ((<**>))
import Control.Concurrent.MVar (newEmptyMVar)
import Data.Function ((&))
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import Network.Wai.Handler.Warp (
  Port,
  Settings,
  defaultSettings,
  runSettings,
  setLogger,
  setPort,
 )
import Network.Wai.Logger (withStdoutLogger)
import Options.Applicative qualified as Options
import System.Exit (die)
import Types (Env (Env, options, status), ServerOptions (ServerOptions, port))

main :: IO ()
main = do
  serverOptions@ServerOptions {port} <- Options.execParser opts
  withStdoutLogger $ \logger -> do
    putStrLn $ "Plutip server starting on port " <> show port
    runSettings (mkSettings port logger)
      . app
      =<< either die pure
      =<< newEnvIO serverOptions
  where
    mkSettings ::
      Port -> (Request -> Status -> Maybe Integer -> IO ()) -> Settings
    mkSettings port logger = defaultSettings & setPort port & setLogger logger

newEnvIO :: ServerOptions -> IO (Either String Env)
newEnvIO options = do
  status <- newEmptyMVar
  pure . Right $ Env {status, options}

opts :: Options.ParserInfo ServerOptions
opts =
  Options.info (serverOptionsParser <**> Options.helper) $
    Options.fullDesc
      <> Options.progDesc
        "plutip-server is used for integration with cardano-transaction-lib"

serverOptionsParser :: Options.Parser ServerOptions
serverOptionsParser =
  ServerOptions
    <$> Options.option
      Options.auto
      ( Options.long "port"
          <> Options.short 'p'
          <> Options.help "Server port"
          <> Options.showDefault
          <> Options.value 8082
          <> Options.metavar "INT"
      )
