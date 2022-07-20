module Test.Ogmios.Aeson
  ( main
  , suite
  ) where

import Prelude

import Aeson (class DecodeAeson, Aeson, printJsonDecodeError)
import Aeson as Aeson
import Foreign.Object (Object)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes, elem, groupAllBy, nubBy)
import Data.Array.NonEmpty (head, length, tail)
import Data.Bifunctor (lmap, bimap)
import Data.Either (hush)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect (Effect)
import Mote (group, skip, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath, basename, concat)
import Node.Process (lookupEnv)
import QueryM.Ogmios as O
import TestM (TestPlanM)
import Test.Utils as Utils
import Type.Proxy (Proxy(Proxy))

supported :: Array String
supported =
  [ "chainTip"
  , "utxo"
  , "currentEpoch"
  , "systemStart"
  , "eraSummaries"
  , "currentProtocolParameters"
  , "SubmitTx"
  -- TODO Support plutus:v2 parameters
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/567
  -- , "currentProtocolParameters-noPlutusV1"
  ]

getField
  :: forall (a :: Type). DecodeAeson a => String -> Object Aeson -> Maybe a
getField f o = join $ hush $ Aeson.getFieldOptional' o f

type Query = String

-- Given a query and an response of the query, create a special case query
specialize :: Query -> Aeson -> Query
specialize query a
  | Just _ :: _ Aeson <- getField "eraMismatch" =<< Aeson.toObject a = query
      <> "-"
      <> "eraMismatch"
  | "currentProtocolParameters" <- query
  , Just costModels <- getField "costModels" =<< Aeson.toObject a
  , Nothing :: _ Aeson <- getField "plutus:v1" costModels = query <> "-" <>
      "noPlutusV1"
  | "SubmitTx" <- query
  , Just _ :: _ Aeson <- getField "SubmitFail" =<< Aeson.toObject a = query
      <> "-"
      <> "SubmitFail"
specialize query _ = query

readdir' :: FilePath -> Aff (Array FilePath)
readdir' fp = (map <<< map) (\fn -> concat [ fp, fn ]) (readdir fp)

applyTuple
  :: forall (a :: Type) (b :: Type) (c :: Type)
   . (a -> b) /\ (a -> c)
  -> a
  -> b /\ c
applyTuple (f /\ g) a = f a /\ g a

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
    catMaybes <$> flip parTraverse (ourFixtures <> ogmiosFixtures) \fp -> do
      let bn = basename fp
      contents <- readTextFile UTF8 fp
      aeson <- liftEither $ lmap
        (error <<< ((bn <> "\n  ") <> _) <<< printJsonDecodeError)
        (Aeson.parseJsonStringToAeson contents)
      pure case pattern >>= flip match bn >>> map tail of
        Just [ Just query ] -> Just
          { query: specialize query aeson
          , bn
          , aeson
          }
        _ -> Nothing

  let
    groupedFiles =
      map (applyTuple $ _.query <<< head /\ map \{ aeson, bn } -> { aeson, bn })
        $ groupAllBy (comparing _.query)
        $ nubBy (comparing _.bn) files

  for_ groupedFiles \(query /\ files') ->
    (if query `elem` supported then identity else skip)
      $ test (query <> " (" <> show (length files') <> ")")
      $
        for_ files' \{ aeson, bn } -> do
          let
            handle :: forall (a :: Type). DecodeAeson a => Proxy a -> Aff Unit
            handle _ = liftEither $ bimap
              ( error <<< ((bn <> "\n  ") <> _) <<<
                  printJsonDecodeError
              )
              (const unit)
              (Aeson.decodeAeson aeson :: _ a)
          case query of
            "chainTip" -> handle (Proxy :: _ O.ChainTipQR)
            "utxo" -> handle (Proxy :: _ O.UtxoQR)
            "currentEpoch" -> handle (Proxy :: _ O.CurrentEpoch)
            "systemStart" -> handle (Proxy :: _ O.SystemStart)
            "eraSummaries" -> handle (Proxy :: _ O.EraSummaries)
            "currentProtocolParameters" -> handle
              (Proxy :: _ O.ProtocolParameters)
            "EvaluateTx" -> handle (Proxy :: _ O.TxEvaluationR)
            "SubmitTx" -> handle (Proxy :: _ O.SubmitTxR)
            _ -> liftEffect $ throw $ "Unknown case " <> bn

main :: Effect Unit
main = launchAff_ do
  Utils.interpret suite
