{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Api (app)
import Control.Applicative ((<**>))
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
import Types (ServerOptions (ServerOptions, port), newEnvIO)

main :: IO ()
main = do
  ServerOptions {port} <- Options.execParser opts
  withStdoutLogger $ \logger -> do
    putStrLn $ "CBTx server starting on port " <> show port
    runSettings (mkSettings port logger)
      . app
      =<< either die pure
      =<< newEnvIO
  where
    mkSettings ::
      Port -> (Request -> Status -> Maybe Integer -> IO ()) -> Settings
    mkSettings port logger = defaultSettings & setPort port & setLogger logger

opts :: Options.ParserInfo ServerOptions
opts =
  Options.info (serverOptions <**> Options.helper) $
    Options.fullDesc <> Options.progDesc "CBTx server"

serverOptions :: Options.Parser ServerOptions
serverOptions =
  ServerOptions
    <$> Options.option
      Options.auto
      ( Options.long "port"
          <> Options.help "Server port"
          <> Options.showDefault
          <> Options.value 8081
          <> Options.metavar "INT"
      )
