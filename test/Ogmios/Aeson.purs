module Test.Ctl.Ogmios.Aeson
  ( main
  , suite
  ) where

import Prelude

import Aeson (Aeson, JsonDecodeError, encodeAeson, printJsonDecodeError)
import Aeson as Aeson
import Contract.Backend.Ogmios.Mempool (MempoolSizeAndCapacity)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Ctl.Internal.QueryM.JsonRpc2
  ( class DecodeOgmios
  , OgmiosDecodeError(ErrorResponse)
  , decodeOgmios
  )
import Ctl.Internal.QueryM.Ogmios
  ( HasTxR
  , SubmitTxR
  , TxEvaluationR
  , aesonObject
  )
import Ctl.Internal.QueryM.Ogmios as O
import Data.Array (catMaybes, groupAllBy, nubBy)
import Data.Array.NonEmpty (NonEmptyArray, head, length, tail)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), hush)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (null, toLower) as String
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign.Object (update) as Object
import Mote (group, skip, test)
import Mote.TestPlanM (TestPlanM, interpret)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath, basename, concat)
import Node.Process (lookupEnv)
import Type.Proxy (Proxy(Proxy))

type Check = String -> Aeson -> Aff Unit

-- These fixtures are tested to decode to given types.
-- Pay attention to update this if fixture filenames change, otherwise tests are going to be ignored.
tested :: Array (String /\ Check)
tested =
  [ ("queryNetwork/tip" /\ check (Proxy :: _ O.ChainTipQR))
  , ("queryNetwork/startTime" /\ check (Proxy :: _ O.CurrentEpoch))
  , ("queryLedgerState/epoch" /\ check (Proxy :: _ O.OgmiosSystemStart))
  , ("queryLedgerState/eraSummaries" /\ check (Proxy :: _ O.OgmiosEraSummaries))
  , ( "queryLedgerState/protocolParameters" /\ check
        (Proxy :: _ O.OgmiosProtocolParameters)
    )
  , ("queryLedgerState/stakePools" /\ check (Proxy :: _ O.PoolParametersR))
  , ( "queryLedgerState/rewardAccountSummaries" /\ check
        (Proxy :: _ O.DelegationsAndRewardsR)
    )
  , ("evaluateTransaction" /\ check (Proxy :: _ TxEvaluationR))
  , ("submitTransaction" /\ check (Proxy :: _ SubmitTxR))
  , ("hasTransaction" /\ check (Proxy :: _ HasTxR))
  , ("sizeOfMempool" /\ check (Proxy :: _ MempoolSizeAndCapacity))
  -- ignoring because response may lack tx cbor if not run with flag
  -- This endpoint is tested with "fetchMempoolTXs" test (Test.Ctl.Plutip.Contract.OgmiosMempool)
  -- , ("nextTransaction" /\ (Proxy :: _ MaybeMempoolTransaction ))
  ]

-- Fixtures from ogmios repo have id set to "null", but we require it as string.
addIdFieldHack :: Aeson -> Either JsonDecodeError Aeson
addIdFieldHack = aesonObject $
  ( pure <<< encodeAeson <<< Object.update
      (const $ pure $ encodeAeson "My favourite id")
      "id"
  )

-- Fail if we can't decode positive result
check
  :: forall (a :: Type)
   . DecodeOgmios a
  => Proxy a
  -> String
  -> Aeson
  -> Aff Unit
check _ bn aeson = liftEither $ lmap
  (error <<< ((bn <> "\n  ") <> _))
  ( do
      aeson' <- lmap show (addIdFieldHack aeson)
      case decodeOgmios aeson' of
        -- we don't decode every error response, that's an expected fail
        Left (ErrorResponse (Just _)) -> pure unit
        Right (_ :: a) -> pure unit
        Left e -> Left $ show e
  )

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
      -- ignore empty (corrupted) fixtures
      if String.null contents then pure Nothing
      else do
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
  let
    (tested' :: Map.Map String Check) = Map.fromFoldable $ map
      (\(q /\ c) -> (String.toLower q /\ c))
      tested

  for_ groupedFiles \(query /\ files') ->
    let
      query' = String.toLower query
    in
      let
        test' ch = test (query <> " (" <> show (length files') <> ")") $ for_
          files'
          \{ aeson, bn } -> ch bn aeson
      in
        case Map.lookup query' tested' of
          Nothing -> skip $ test' (\_ _ -> pure unit)
          Just check' -> test' check'

main :: Effect Unit
main = launchAff_ do
  interpret suite
