module Test.Webpack where

import Effect.Aff
import Effect.Aff.Compat
import Effect.Uncurried
import Prelude

import Effect (Effect)
import Node.Path (FilePath)

rootDir :: FilePath
rootDir = "."

tmpDir :: FilePath
tmpDir = "/"

-- throws error or returns stdout
foreign import _exec :: String -> EffectFnAff String

exec :: String -> Aff String
exec cmd = fromEffectFnAff $ _exec cmd

bundleExampleTo :: String -> FilePath -> Aff Unit
bundleExampleTo example filepath =
  let
    cmd = "spago bundle-module -m " <> example <> " --to " <> filepath
  in
    void $ exec cmd

foreign import _runServer :: Int -> EffectFnAff Unit

runServer :: Int -> Aff Unit
runServer port = fromEffectFnAff $ _runServer port

runWebpackServer :: String -> FilePath -> Aff Unit
runWebpackServer example filepath = do
  bundleExampleTo example filepath
  runServer 4009

