module Test.OgmiosDatumCache
  ( suite
  ) where

import Prelude

import Aeson (caseAesonArray, decodeAeson, encodeAeson)
import Contract.Address (ByteArray)
import Control.Monad.Error.Class (class MonadThrow)
import Ctl.Internal.Test.Utils (TestPlanM, errEither, errMaybe, readAeson)
import Data.Either (Either(Right, Left))
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Hashing (datumHash)
import Mote (group, skip, test)
import Test.Spec.Assertions (shouldEqual)
import Types.Datum (Datum(Datum))
import Types.PlutusData (PlutusData)

suite :: TestPlanM (Aff Unit) Unit
suite = group "Ogmios Datum Cache tests" $ do
  skip $ test
    "Plutus data samples should satisfy the Aeson roundtrip test (FIXME: \
    \https://github.com/mlabs-haskell/purescript-aeson/issues/7)"
    plutusDataToFromAesonTest
  test "Plutus data samples should have a compatible hash" plutusDataHashingTest

readPlutusDataSamples
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => m (Array { hash :: ByteArray, plutusData :: PlutusData })
readPlutusDataSamples = do
  errEither <<< decodeAeson =<< readAeson
    "./fixtures/test/ogmios-datum-cache/plutus-data-samples.json"

plutusDataToFromAesonTest
  :: forall (m :: Type -> Type). MonadEffect m => MonadThrow Error m => m Unit
plutusDataToFromAesonTest = do
  pdsAes <- readAeson
    "./fixtures/test/ogmios-datum-cache/plutus-data-samples.json"
  aess <- errEither <<< caseAesonArray (Left "Expected a Json array") Right $
    pdsAes
  for_ aess \aes -> do
    (sample :: { hash :: ByteArray, plutusData :: PlutusData }) <- errEither $
      decodeAeson aes
    let aes' = encodeAeson sample
    aes `shouldEqual` aes'

plutusDataHashingTest
  :: forall (m :: Type -> Type). MonadEffect m => MonadThrow Error m => m Unit
plutusDataHashingTest = do
  plutusDataSamples <- readPlutusDataSamples
  let elems = plutusDataSamples
  for_ elems \{ hash, plutusData } -> do
    hash' <- errMaybe "Couldn't hash the datum" <<< datumHash $ Datum plutusData
    hash `shouldEqual` unwrap hash'
