module Main (main) where

import Api
import Network.Wai.Handler.Warp (runSettings)
import Network.Wai.Logger (withStdoutLogger)

main :: IO ()
main = withStdoutLogger $ \logger ->
  runSettings (mkSettings logger) . app $ undefined -- TODO new env
  where
    mkSettings = undefined
