module Test.Ctl.Blockfrost.GenerateFixtures.ChainTip (main) where

import Prelude

import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostEndpoint(LatestBlock)
  , BlockfrostRawResponse
  , runBlockfrostServiceTestM
  )
import Ctl.Internal.Service.Blockfrost (getChainTip) as Blockfrost
import Ctl.Internal.Types.Chain (Tip)
import Data.Either (hush)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(Milliseconds), delay, launchAff_)
import Effect.Class (liftEffect)
import Test.Ctl.Blockfrost.GenerateFixtures.Helpers
  ( blockfrostBackend
  , storeBlockfrostFixture
  )

main :: Effect Unit
main = launchAff_ (generateFixtures 10)

generateFixtures :: Int -> Aff Unit
generateFixtures numFixtures = do
  backend <- liftEffect blockfrostBackend
  worker backend zero Nothing
  where
  worker :: BlockfrostBackend -> Int -> Maybe Tip -> Aff Unit
  worker _ i _ | i == numFixtures = pure unit
  worker backend i prevChainTip = do
    chainTip <- runBlockfrostServiceTestM (\_ -> pure unit) backend
      (Just onBlockfrostRawResponse)
      Nothing
      Blockfrost.getChainTip
    case prevChainTip == hush chainTip of
      true -> delay (Milliseconds 5000.0) *> worker backend i (hush chainTip)
      false -> worker backend (i + 1) (hush chainTip)
    where
    onBlockfrostRawResponse
      :: BlockfrostEndpoint -> BlockfrostRawResponse -> Aff Unit
    onBlockfrostRawResponse query rawResponse =
      case query of
        LatestBlock -> storeBlockfrostFixture i "getChainTip" rawResponse
        _ -> pure unit
