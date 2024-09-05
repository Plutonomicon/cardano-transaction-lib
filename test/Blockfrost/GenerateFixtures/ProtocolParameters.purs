module Test.Ctl.Blockfrost.GenerateFixtures.ProtocolParameters
  ( main
  ) where

import Prelude

import Ctl.Internal.Service.Blockfrost
  ( BlockfrostEndpoint(LatestProtocolParameters)
  , BlockfrostRawResponse
  , runBlockfrostServiceTestM
  )
import Ctl.Internal.Service.Blockfrost (getProtocolParameters) as Blockfrost
import Data.Either (either)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (take) as String
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
  backend@{ blockfrostApiKey } <- liftEffect blockfrostBackend
  let
    onBlockfrostRawResponse
      :: BlockfrostEndpoint
      -> BlockfrostRawResponse
      -> Aff Unit
    onBlockfrostRawResponse query rawResp =
      case query of
        LatestProtocolParameters ->
          storeBlockfrostFixture
            zero
            ( "getProtocolParameters" <> maybe mempty (append "-")
                (networkNameFromApiKey <$> blockfrostApiKey)
            )
            rawResp
        _ ->
          pure unit
  pparams <- runBlockfrostServiceTestM (const (pure unit)) backend
    (Just onBlockfrostRawResponse)
    Nothing
    Blockfrost.getProtocolParameters
  either (liftEffect <<< throw <<< show) (const (pure unit))
    pparams

networkNameFromApiKey :: String -> String
networkNameFromApiKey =
  String.take 7 >>> case _ of
    "sanchon" -> "sanchonet"
    network -> network
