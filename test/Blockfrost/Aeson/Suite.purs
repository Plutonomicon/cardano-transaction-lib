module Test.Ctl.Blockfrost.Aeson.Suite (main, suite) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError
  , decodeAeson
  , parseJsonStringToAeson
  , printJsonDecodeError
  )
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostCurrentEpoch
  , BlockfrostMetadata
  , BlockfrostProtocolParameters
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.Array (catMaybes, length)
import Data.Array.NonEmpty (tail)
import Data.Bifunctor (bimap, lmap)
import Data.Bounded.Generic (genericBottom)
import Data.Either (Either, hush)
import Data.Enum.Generic (genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Mote (group, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath, basename, concat)
import Type.Proxy (Proxy(Proxy))

main :: Effect Unit
main = launchAff_ (interpret suite)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Blockfrost Aeson tests" (tests (Just genericBottom))
  where
  tests :: Maybe Query -> TestPlanM (Aff Unit) Unit
  tests Nothing = pure unit
  tests (Just query) = do
    fixtures <- lift $ loadFixtures (printQuery query)
    test (printQuery query <> " (" <> show (length fixtures) <> ")") do
      for_ fixtures \{ aeson, bn } -> do
        let
          handle :: forall (a :: Type). DecodeAeson a => Proxy a -> Aff Unit
          handle _ = liftEither $ bimap
            (error <<< ((bn <> "\n ") <> _) <<< printJsonDecodeError)
            (const unit)
            (decodeAeson aeson :: Either JsonDecodeError a)
        case query of
          GetTxMetadataQuery -> handle (Proxy :: Proxy BlockfrostMetadata)
          GetCurrentEpochQuery -> handle (Proxy :: Proxy BlockfrostCurrentEpoch)
          GetProtocolParametersQuery -> handle
            (Proxy :: Proxy BlockfrostProtocolParameters)
    tests (genericSucc query)

data Query
  = GetTxMetadataQuery
  | GetCurrentEpochQuery
  | GetProtocolParametersQuery

derive instance Generic Query _

printQuery :: Query -> String
printQuery = case _ of
  GetTxMetadataQuery -> "getTxMetadata"
  GetCurrentEpochQuery -> "getCurrentEpoch"
  GetProtocolParametersQuery -> "getProtocolParameters"

loadFixtures :: FilePath -> Aff (Array { aeson :: Aeson, bn :: String })
loadFixtures query = do
  files <- readdir' path
  catMaybes <$> flip parTraverse files \filepath -> do
    let bn = basename filepath
    case pattern >>= flip match bn >>> map tail of
      Just [ Just query' ] | query' == query -> do
        contents <- readTextFile UTF8 filepath
        aeson <- liftEither $ lmap
          (error <<< ((bn <> "\n ") <> _) <<< printJsonDecodeError)
          (parseJsonStringToAeson contents)
        pure $ Just { aeson, bn }
      _ -> pure Nothing
  where
  path :: FilePath
  path = concat [ "fixtures", "test", "blockfrost", query ]

  pattern :: Maybe Regex
  pattern = hush $ regex "^([a-zA-Z]+)-[0-9a-fA-F]+\\.json$" noFlags

readdir' :: FilePath -> Aff (Array FilePath)
readdir' fp = (map <<< map) (\fn -> concat [ fp, fn ]) (readdir fp)
