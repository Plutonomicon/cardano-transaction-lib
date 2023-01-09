module Test.Ctl.Blockfrost.Aeson
  ( main
  , suite
  ) where

import Prelude

import Aeson (class DecodeAeson, Aeson, printJsonDecodeError)
import Aeson as Aeson
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Ctl.Internal.Service.Blockfrost as B
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.Array (catMaybes, groupAllBy, nubBy)
import Data.Array.NonEmpty (NonEmptyArray, head, length, tail)
import Data.Bifunctor (bimap, lmap)
import Data.Either (hush)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath, basename, concat)
import Type.Proxy (Proxy(Proxy))

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
    path = concat [ "fixtures", "test", "blockfrost" ]
    pattern = hush $ regex "^([a-zA-Z]+)-[0-9a-fA-F]+\\.json$" noFlags

  files <- do
    ourFixtures <- readdir' path
    catMaybes <$> flip parTraverse ourFixtures \fp -> do
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
suite = group "Blockfrost Aeson tests" do
  groupedFiles <- lift loadFixtures

  for_ groupedFiles \(query /\ files') ->
    test (query <> " (" <> show (length files') <> ")")
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
            "getTxMetadata" -> handle (Proxy :: _ B.BlockfrostMetadata)
            _ -> liftEffect $ throw $ "Unknown case " <> bn

main :: Effect Unit
main = launchAff_ do
  interpret suite
