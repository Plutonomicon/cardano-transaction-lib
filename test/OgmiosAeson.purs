module Test.OgmiosAeson where

import Prelude

import Aeson (class DecodeAeson, decodeJsonString)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (catMaybes, groupAllBy)
import Data.Array.NonEmpty (head, tail, unzip)
import Data.Bifunctor (bimap)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (for_)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, test)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (concat)
import QueryM.Ogmios as O
import TestM (TestPlanM)
import Type.Proxy (Proxy(..))

suite :: TestPlanM Unit
suite = group "Ogmios Aeson tests" do
  let
    path = concat [ "fixtures", "test", "ogmios" ]
    pattern = hush $ regex "^([a-zA-Z]+)-[0-9a-fA-F]+\\.json$" noFlags
  files <- lift $ readdir path

  let
    groupedFiles = map (first head <<< unzip)
      $ groupAllBy (\a b -> compare (fst a) (fst b))
      $ catMaybes
      $ flip map files \fp -> case pattern >>= flip match fp >>> map tail of
          Just [ Just query ] -> Just (query /\ fp)
          _ -> Nothing

  for_ groupedFiles \(query /\ fps) ->
    test query $
      for_ fps \fp -> do
        file <- readTextFile UTF8 (concat [ path, fp ])
        let
          handle :: forall f a. DecodeAeson a => f a -> Aff Unit
          handle _ = liftEither $ bimap
            (error <<< ((fp <> "\n  ") <> _) <<< printJsonDecodeError)
            (const unit)
            (decodeJsonString file :: _ a)
        case query of
          "chainTip" -> handle (Proxy :: _ O.ChainTipQR)
          "utxosAt" -> handle (Proxy :: _ O.UtxoQR)
          "currentEpoch" -> handle (Proxy :: _ O.CurrentEpoch)
          "systemStart" -> handle (Proxy :: _ O.SystemStart)
          "eraSummaries" -> handle (Proxy :: _ O.EraSummaries)
          "currentProtocolParameters" -> handle
            (Proxy :: _ O.ProtocolParameters)
          _ -> liftEffect $ throw $ "Unknown case " <> fp
