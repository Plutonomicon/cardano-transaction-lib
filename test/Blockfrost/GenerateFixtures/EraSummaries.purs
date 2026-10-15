module Test.Ctl.Blockfrost.GenerateFixtures.EraSummaries (main) where

import Prelude

import Cardano.Blockfrost.Service
  ( BlockfrostEndpoint(EraSummaries)
  , BlockfrostRawResponse
  , runBlockfrostServiceTestM
  )
import Cardano.Blockfrost.Service (getEraSummaries) as Blockfrost
import Data.Either (either)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Test.Ctl.Blockfrost.GenerateFixtures.Helpers
  ( blockfrostBackend
  , storeBlockfrostFixture
  )

main :: Effect Unit
main = launchAff_ generateFixture

generateFixture :: Aff Unit
generateFixture = do
  backend <- liftEffect blockfrostBackend
  eraSummaries <- runBlockfrostServiceTestM (\_ -> pure unit) backend
    (Just onBlockfrostRawResponse)
    Nothing
    Blockfrost.getEraSummaries
  either (liftEffect <<< throw <<< show) (\_ -> pure unit) eraSummaries
  where
  onBlockfrostRawResponse
    :: BlockfrostEndpoint -> BlockfrostRawResponse -> Aff Unit
  onBlockfrostRawResponse query rawResponse =
    case query of
      EraSummaries ->
        storeBlockfrostFixture zero "getEraSummaries" rawResponse
      _ -> pure unit
