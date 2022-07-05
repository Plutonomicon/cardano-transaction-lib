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
import Ogmios.Query qualified
import Options.Applicative qualified as Options
import System.Exit (die)
import Types (ServerOptions (ServerOptions, port), newEnvIO)

main :: IO ()
main = do
  serverOptions@ServerOptions {port} <- Options.execParser opts
  withStdoutLogger $ \logger -> do
    putStrLn $ "CTL server starting on port " <> show port
    runSettings (mkSettings port logger)
      . app
      =<< either die pure
      =<< newEnvIO serverOptions
  where
    mkSettings ::
      Port -> (Request -> Status -> Maybe Integer -> IO ()) -> Settings
    mkSettings port logger = defaultSettings & setPort port & setLogger logger

opts :: Options.ParserInfo ServerOptions
opts =
  Options.info (serverOptionsParser <**> Options.helper) $
    Options.fullDesc
      <> Options.progDesc
        "CBTx server. See the README for routes and request/response types"

serverOptionsParser :: Options.Parser ServerOptions
serverOptionsParser =
  ServerOptions
    <$> Options.option
      Options.auto
      ( Options.long "port"
          <> Options.short 'p'
          <> Options.help "Server port"
          <> Options.showDefault
          <> Options.value 8081
          <> Options.metavar "INT"
      )
    <*> Options.option
      Options.str
      ( Options.long "ogmios-host"
          <> Options.help "The hostname for ogmios"
          <> Options.showDefault
          <> Options.value
            ( Ogmios.Query.host
                Ogmios.Query.defaultServerParameters
            )
          <> Options.metavar "IPV4"
      )
    <*> Options.option
      Options.auto
      ( Options.long "ogmios-port"
          <> Options.help "The port for ogmios"
          <> Options.showDefault
          <> Options.value
            ( Ogmios.Query.port
                Ogmios.Query.defaultServerParameters
            )
          <> Options.metavar "INT"
      )
