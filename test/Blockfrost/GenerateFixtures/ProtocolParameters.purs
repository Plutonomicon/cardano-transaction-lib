module Test.Ctl.Blockfrost.GenerateFixtures.ProtocolParameters (main) where

import Prelude

import Ctl.Internal.Service.Blockfrost
  ( BlockfrostEndpoint(LatestProtocolParameters)
  , BlockfrostRawResponse
  , runBlockfrostServiceTestM
  )
import Ctl.Internal.Service.Blockfrost (getProtocolParameters) as Blockfrost
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
  pparams <- runBlockfrostServiceTestM (const (pure unit)) backend
    (Just onBlockfrostRawResponse)
    Nothing
    Blockfrost.getProtocolParameters
  either (liftEffect <<< throw <<< show) (const (pure unit))
    pparams
  where
  onBlockfrostRawResponse
    :: BlockfrostEndpoint
    -> BlockfrostRawResponse
    -> Aff Unit
  onBlockfrostRawResponse query rawResp =
    case query of
      LatestProtocolParameters ->
        storeBlockfrostFixture zero "getProtocolParameters" rawResp
      _ -> pure unit
