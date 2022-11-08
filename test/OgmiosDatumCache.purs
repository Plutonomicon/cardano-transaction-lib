module Test.Ctl.OgmiosDatumCache
  ( suite
  ) where

import Prelude

import Aeson (caseAesonArray, decodeAeson, encodeAeson)
import Contract.Address (ByteArray)
import Control.Monad.Error.Class (class MonadThrow)
import Ctl.Internal.Hashing (datumHash)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.Datum (Datum(Datum))
import Ctl.Internal.Types.PlutusData (PlutusData)
import Data.Either (Either(Right, Left))
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Mote (group, skip, test)
import Node.Path (FilePath)
import Test.Ctl.Utils (errEither, errMaybe, readAeson)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = group "Ogmios Datum Cache tests" $ do
  let
    samplesPath = "plutus-data-samples.json"
    ogmiosResponsePath = "get-datums-by-hashes-samples.json"
  -- plutus-data-samples
  skip $ test
    "Plutus data samples should satisfy the Aeson roundtrip test (FIXME: \
    \https://github.com/mlabs-haskell/purescript-aeson/issues/7)"
    (plutusDataToFromAesonTest samplesPath)

  test "Plutus data samples should have a compatible hash"
    (plutusDataHashingTest samplesPath)

  -- ogmios responses
  skip $ test
    "Ogmios response samples should satisfy the Aeson roundtrip test (FIXME: \
    \https://github.com/mlabs-haskell/purescript-aeson/issues/7)"
    (plutusDataToFromAesonTest ogmiosResponsePath)

  test "Ogmios response samples should have a compatible hash"
    (plutusDataHashingTest ogmiosResponsePath)

-- mkDataToFromAesonTest :: FilePath -> String ->

readPlutusDataSamples
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => FilePath
  -> m (Array { hash :: ByteArray, plutusData :: PlutusData })
readPlutusDataSamples path = do
  errEither <<< decodeAeson =<< readAeson
    ("./fixtures/test/ogmios-datum-cache/" <> path)

plutusDataToFromAesonTest
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => MonadThrow Error m
  => FilePath
  -> m Unit
plutusDataToFromAesonTest path = do
  pdsAes <- readAeson
    $ "./fixtures/test/ogmios-datum-cache/" <> path
  aess <- errEither <<< caseAesonArray (Left "Expected a Json array") Right $
    pdsAes
  for_ aess \aes -> do
    (sample :: { hash :: ByteArray, plutusData :: PlutusData }) <- errEither $
      decodeAeson aes
    let aes' = encodeAeson sample
    aes `shouldEqual` aes'

plutusDataHashingTest
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => MonadThrow Error m
  => FilePath
  -> m Unit
plutusDataHashingTest path = do
  plutusDataSamples <- readPlutusDataSamples path
  let elems = plutusDataSamples
  for_ elems \{ hash, plutusData } -> do
    hash' <- errMaybe "Couldn't hash the datum" <<< datumHash $ Datum plutusData
    hash `shouldEqual` unwrap hash'
