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
import System.IO (hFlush, stdout)
import Types (ServerOptions (ServerOptions, port))

main :: IO ()
main = do
  ServerOptions {port} <- Options.execParser opts
  withStdoutLogger $ \logger -> do
    putStrLn $ "CTL server starting on port " <> show port
    hFlush stdout
    runSettings (mkSettings port logger) app
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
