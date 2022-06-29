module Test.OgmiosAeson where

import Prelude

import Aeson (class DecodeAeson, decodeJsonString)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (catMaybes, elem, filter, groupAllBy, notElem)
import Data.Array.NonEmpty (head, tail, unzip)
import Data.Bifunctor (bimap)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Strong (first)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (for_)
import Data.Tuple (fst)
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
import Type.Proxy (Proxy(..))

supported :: Array String
supported =
  [ "chainTip"
    -- TODO Merge
  , "utxosAt"
  , "utxo"
  , "currentEpoch"
  , "systemStart"
  , "eraSummaries"
  , "currentProtocolParameters"
  ]

-- Some fixtures we intend to never support
blacklist :: Array String
blacklist =
  [ -- plutusv1 support is expected
    "currentProtocolParameters-00480fa51517b5af4b7bf1779e8268e4.json"
  ]

readdir' :: FilePath -> Aff (Array FilePath)
readdir' fp = (map <<< map) (\fn -> concat [fp, fn]) (readdir fp)

suite :: TestPlanM Unit
suite = group "Ogmios Aeson tests" do
  let
    path = concat [ "fixtures", "test", "ogmios" ]
    pattern = hush $ regex "^([a-zA-Z]+)-[0-9a-fA-F]+\\.json$" noFlags

  -- TODO Fail if no OGMIOS_FIXTURES
  files <- lift do
    ourFixtures <- readdir' path
    ogmiosFixtures <- (liftEffect $ lookupEnv "OGMIOS_FIXTURES") >>= maybe (pure []) readdir'
    pure (filter (\fp -> basename fp `notElem` blacklist) $ ourFixtures <> ogmiosFixtures)

  let
    groupedFiles = map (first head <<< unzip)
      $ groupAllBy (\a b -> compare (fst a) (fst b))
      $ catMaybes
      $ flip map files \fp -> case pattern >>= flip match (basename fp) >>> map tail of
          Just [ Just query ] -> Just (query /\ fp)
          _ -> Nothing

  for_ groupedFiles \(query /\ fps) ->
    (if query `elem` supported then identity else skip) $ test query $
      for_ fps \fp -> do
        file <- readTextFile UTF8 fp
        let
          handle :: forall f a. DecodeAeson a => f a -> Aff Unit
          handle _ = liftEither $ bimap
            (error <<< ((basename fp <> "\n  ") <> _) <<< printJsonDecodeError)
            (const unit)
            (decodeJsonString file :: _ a)
        case query of
          "chainTip" -> handle (Proxy :: _ O.ChainTipQR)
          -- TODO Merge
          "utxosAt" -> handle (Proxy :: _ O.UtxoQR)
          "utxo" -> handle (Proxy :: _ O.UtxoQR)
          "currentEpoch" -> handle (Proxy :: _ O.CurrentEpoch)
          "systemStart" -> handle (Proxy :: _ O.SystemStart)
          "eraSummaries" -> handle (Proxy :: _ O.EraSummaries)
          "currentProtocolParameters" -> handle
            (Proxy :: _ O.ProtocolParameters)
          _ -> liftEffect $ throw $ "Unknown case " <> basename fp
