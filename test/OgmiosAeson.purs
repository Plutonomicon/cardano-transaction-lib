module Test.OgmiosAeson where

import Prelude

import Aeson (class DecodeAeson, decodeJsonString)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (catMaybes, elem, filter, groupAllBy, notElem, nubBy)
import Data.Array.NonEmpty (head, length, tail, unzip)
import Data.Bifunctor (bimap)
import Data.Either (hush)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Profunctor.Strong (first)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (for_)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, skip, test)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath, basename, concat)
import Node.Process (lookupEnv)
import QueryM.Ogmios as O
import TestM (TestPlanM)
import Type.Proxy (Proxy(Proxy))

supported :: Array String
supported =
  [ "chainTip"
  , "utxo"
  , "currentEpoch"
  , "systemStart"
  , "eraSummaries"
  , "currentProtocolParameters"
  ]

-- Some fixtures using features we don't yet support
blacklist :: Array String
blacklist =
  [ -- EraMismatch
    "currentProtocolParameters-a9df54ac55c8156d849806393b2c490d.json"
  , "currentProtocolParameters-f7482203ea6d735521aaa13d3bdbcc8c.json"
  , "utxo-a9df54ac55c8156d849806393b2c490d.json"
  , "utxo-f7482203ea6d735521aaa13d3bdbcc8c.json"
  ]

readdir' :: FilePath -> Aff (Array FilePath)
readdir' fp = (map <<< map) (\fn -> concat [ fp, fn ]) (readdir fp)

suite :: TestPlanM Unit
suite = group "Ogmios Aeson tests" do
  let
    path = concat [ "fixtures", "test", "ogmios" ]
    pattern = hush $ regex "^([a-zA-Z]+)-[0-9a-fA-F]+\\.json$" noFlags

  files <- lift do
    ourFixtures <- readdir' path
    ogmiosFixtures <- (liftEffect $ lookupEnv "OGMIOS_FIXTURES") >>= maybe
      (liftEffect $ throw $ "OGMIOS_FIXTURES environment variable not set")
      readdir'
    pure
      ( filter (\fp -> basename fp `notElem` blacklist) $ ourFixtures <>
          ogmiosFixtures
      )

  let
    groupedFiles = map (first head <<< unzip)
      $ groupAllBy (comparing fst)
      $ nubBy (comparing (basename <<< snd))
      $ catMaybes
      $ flip map files \fp ->
          case pattern >>= flip match (basename fp) >>> map tail of
            Just [ Just query ] -> Just (query /\ fp)
            _ -> Nothing

  for_ groupedFiles \(query /\ fps) ->
    (if query `elem` supported then identity else skip)
      $ test (query <> " (" <> show (length fps) <> ")")
      $
        for_ fps \fp -> do
          file <- readTextFile UTF8 fp
          let
            handle :: forall f a. DecodeAeson a => f a -> Aff Unit
            handle _ = liftEither $ bimap
              ( error <<< ((basename fp <> "\n  ") <> _) <<<
                  printJsonDecodeError
              )
              (const unit)
              (decodeJsonString file :: _ a)
          case query of
            "chainTip" -> handle (Proxy :: _ O.ChainTipQR)
            "utxo" -> handle (Proxy :: _ O.UtxoQR)
            "currentEpoch" -> handle (Proxy :: _ O.CurrentEpoch)
            "systemStart" -> handle (Proxy :: _ O.SystemStart)
            "eraSummaries" -> handle (Proxy :: _ O.EraSummaries)
            "currentProtocolParameters" -> handle
              (Proxy :: _ O.ProtocolParameters)
            _ -> liftEffect $ throw $ "Unknown case " <> basename fp
