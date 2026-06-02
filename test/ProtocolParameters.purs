module Test.Ctl.ProtocolParameters
  ( main
  , suite
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeJsonString)
import Cardano.Blockfrost.BlockfrostProtocolParameters
  ( BlockfrostProtocolParameters(BlockfrostProtocolParameters)
  )
import Cardano.Kupmios.Ogmios.Types
  ( OgmiosProtocolParameters(OgmiosProtocolParameters)
  )
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (lmap)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Mote (group, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (defaultConfig)

-- These fixtures were acquired soon after each other, so we can compare their
-- parsed results

blockfrostPreprodFixture :: String
blockfrostPreprodFixture =
  "blockfrost/getProtocolParameters-preprod/getProtocolParameters-preprod-777ccf0382d07f157254394b89a2bfb1.json"

blockfrostPreviewFixture :: String
blockfrostPreviewFixture =
  "blockfrost/getProtocolParameters-preview/getProtocolParameters-preview-5eb5597b71674414383b4adcf5ba1045.json"

ogmiosPreprodFixture :: String
ogmiosPreprodFixture =
  "ogmios/queryLedgerState-protocolParameters-preprod-aba1f2da221ecf7bf6e002ddd026fb9a.json"

ogmiosPreviewFixture :: String
ogmiosPreviewFixture =
  "ogmios/queryLedgerState-protocolParameters-preview-2467cdc2c839aae9535e26724ac1f592.json"

loadFixture :: forall (a :: Type). DecodeAeson a => String -> Aff a
loadFixture fixture =
  readTextFile UTF8 ("fixtures/test/" <> fixture)
    <#> decodeJsonString >>> lmap (show >>> error)
    >>= liftEither

main :: Effect Unit
main = launchAff_ do
  interpretWithConfig
    defaultConfig
    suite

suite :: TestPlanM (Aff Unit) Unit
suite = group "Blockfrost" do
  testProtocolParameters "preprod"
    { blockfrostFixture: blockfrostPreprodFixture
    , ogmiosFixture: ogmiosPreprodFixture
    }
  testProtocolParameters "preview"
    { blockfrostFixture: blockfrostPreviewFixture
    , ogmiosFixture: ogmiosPreviewFixture
    }

testProtocolParameters
  :: String
  -> { blockfrostFixture :: String, ogmiosFixture :: String }
  -> TestPlanM (Aff Unit) Unit
testProtocolParameters network { blockfrostFixture, ogmiosFixture } = do
  test
    ( "Blockfrost <> Ogmios ProtocolParameters parsing verification ("
        <> network
        <> ")"
    )
    do
      BlockfrostProtocolParameters blockfrostFixture' <- loadFixture
        blockfrostFixture
      { result: OgmiosProtocolParameters ogmiosFixture' }
        :: { result :: OgmiosProtocolParameters } <- loadFixture ogmiosFixture

      blockfrostFixture' `shouldEqual` ogmiosFixture'
