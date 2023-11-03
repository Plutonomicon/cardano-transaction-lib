module Test.Ctl.Ogmios.Aeson
  ( main
  , suite
  ) where

import Prelude

import Aeson (Aeson, printJsonDecodeError)
import Aeson as Aeson
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Ctl.Internal.QueryM.JsonRpc2 (class DecodeOgmios, decodeOgmiosResponse)
import Ctl.Internal.QueryM.Ogmios as O
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.Array (catMaybes, elem, groupAllBy, nubBy)
import Data.Array.NonEmpty (NonEmptyArray, head, length, tail)
import Data.Bifunctor (bimap, lmap)
import Data.Either (hush)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, skip, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath, basename, concat)
import Node.Process (lookupEnv)
import Type.Proxy (Proxy(Proxy))

supported :: Array String
supported =
  [ "queryNetwork/tip"
  , "queryNetwork/startTime"
  , "queryLedgerState/epoch"
  , "queryLedgerState/eraSummaries"
  , "queryLedgerState/protocolParameters"
  , "queryLedgerState/stakePools"
  -- , "queryLedgerState/rewardAccountSummaries"
  -- TODO Support plutus:v2 parameters
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/567
  -- , "currentProtocolParameters-noPlutusV1"
  ]

type Query = String

readdir' :: FilePath -> Aff (Array FilePath)
readdir' fp = (map <<< map) (\fn -> concat [ fp, fn ]) (readdir fp)

applyTuple
  :: forall (a :: Type) (b :: Type) (c :: Type)
   . (a -> b) /\ (a -> c)
  -> a
  -> b /\ c
applyTuple (f /\ g) a = f a /\ g a

loadFixtures
  :: Aff (Array (Query /\ NonEmptyArray { aeson :: Aeson, bn :: String }))
loadFixtures = do
  let
    path = concat [ "fixtures", "test", "ogmios" ]
    pattern = hush $ regex "^([a-zA-Z]+)-[0-9a-fA-F]+\\.json$" noFlags

  files <- do
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
          { query
          , bn
          , aeson
          }
        _ -> Nothing

  let
    groupedFiles =
      map (applyTuple $ _.query <<< head /\ map \{ aeson, bn } -> { aeson, bn })
        $ groupAllBy (comparing _.query)
        $ nubBy (comparing _.bn) files

  pure groupedFiles

suite :: TestPlanM (Aff Unit) Unit
suite = group "Ogmios Aeson tests" do
  groupedFiles <- lift loadFixtures

  for_ groupedFiles \(query /\ files') ->
    (if query `elem` supported then identity else skip)
      $ test (query <> " (" <> show (length files') <> ")")
      $
        for_ files' \{ aeson, bn } -> do
          let
            handle :: forall (a :: Type). DecodeOgmios a => Proxy a -> Aff Unit
            handle _ = liftEither $ bimap
              ( error <<< ((bn <> "\n  ") <> _) <<< show )
              (const unit)
              (decodeOgmiosResponse aeson :: _ a)
          case query of
            "queryNetwork/tip" -> handle (Proxy :: _ O.ChainTipQR)
            "queryLedgerState/epoch" -> handle (Proxy :: _ O.CurrentEpoch)
            "queryNetwork/startTime" -> handle (Proxy :: _ O.OgmiosSystemStart)
            "queryLedgerState/eraSummaries" -> handle
              (Proxy :: _ O.OgmiosEraSummaries)
            "queryLedgerState/protocolParameters" -> handle
              (Proxy :: _ O.OgmiosProtocolParameters)
            "queryLedgerState/stakePools" -> handle
              (Proxy :: _ O.PoolParametersR)
            -- "queryLedgerState/rewardAccountSummaries" -> handle
            --   (Proxy :: _ O.DelegationsAndRewardsR)
            _ -> liftEffect $ throw $ "Unknown case " <> bn

main :: Effect Unit
main = launchAff_ do
  interpret suite
