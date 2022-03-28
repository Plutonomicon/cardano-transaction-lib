module Main (main) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Plutus.V1.Ledger.Api (Script, Validator (Validator))
import Scripts (alwaysSucceeds)

main :: IO ()
main = for_ scripts $ \(name, script) -> do
  putStr $ name <> ": "
  putStrLn . LC8.unpack $ encode script
  where
    scripts :: [(String, Script)]
    scripts = [("AlwaysSucceeds", coerce @_ @Script alwaysSucceeds)]
