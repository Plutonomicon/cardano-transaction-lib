module Test.OgmiosDatumCache
  ( plutusDataToFromAesonTest
  , suite
  ) where

import Prelude

import Aeson (caseAesonArray, decodeAeson, encodeAeson)
import Contract.Address (ByteArray)
import Contract.PlutusData (Datum(..))
import Contract.Prelude (log)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(Right, Left))
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Hashing (datumHash)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (errEither, errMaybe, readAeson)
import TestM (TestPlanM)
import Types.PlutusData (PlutusData)

suite :: TestPlanM Unit
suite = group "Ogmios Datum Cache tests" $ do
  test "Plutus data samples should satisfy the Aeson roundtrip test"
    plutusDataToFromAesonTest
  test "Plutus data samples should have a compatible hash" plutusDataHashingTest

readPlutusDataSamples
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => m (Array { hash :: ByteArray, plutusData :: PlutusData })
readPlutusDataSamples = do
  aes <- readAeson "./fixtures/test/ogmios-datum-cache/plutus-data-samples.json"
  errEither <<< decodeAeson $ aes

plutusDataToFromAesonTest
  ∷ ∀ (m ∷ Type -> Type). MonadEffect m => MonadThrow Error m ⇒ m Unit
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
  ∷ ∀ (m ∷ Type -> Type). MonadEffect m => MonadThrow Error m ⇒ m Unit
plutusDataHashingTest = do
  plutusDataSamples <- readPlutusDataSamples
  let elems = plutusDataSamples
  for_ elems \{ hash, plutusData } -> do
    hash' <- errMaybe "Couldn't hash the datum" <<< datumHash $ Datum plutusData
    log $ show plutusData
    hash `shouldEqual` unwrap hash'
