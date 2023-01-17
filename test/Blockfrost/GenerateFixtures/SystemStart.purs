module Test.Ctl.Blockfrost.GenerateFixtures.SystemStart (main) where

import Prelude

import Ctl.Internal.Service.Blockfrost
  ( BlockfrostEndpoint(BlockchainGenesis)
  , BlockfrostRawResponse
  , runBlockfrostServiceTestM
  )
import Ctl.Internal.Service.Blockfrost (getSystemStart) as Blockfrost
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
  sysStart <- runBlockfrostServiceTestM (\_ -> pure unit) backend
    (Just onBlockfrostRawResponse)
    Nothing
    Blockfrost.getSystemStart
  either (liftEffect <<< throw <<< show) (\_ -> pure unit) sysStart
  where
  onBlockfrostRawResponse
    :: BlockfrostEndpoint -> BlockfrostRawResponse -> Aff Unit
  onBlockfrostRawResponse query rawResponse =
    case query of
      BlockchainGenesis ->
        storeBlockfrostFixture zero "getSystemStart" rawResponse
      _ -> pure unit
