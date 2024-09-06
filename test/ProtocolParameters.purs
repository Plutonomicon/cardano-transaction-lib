module Test.Ctl.ProtocolParameters
  ( main
  , suite
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeJsonString)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Control.Monad.Error.Class (liftEither)
import Ctl.Internal.QueryM.Ogmios
  ( OgmiosProtocolParameters(OgmiosProtocolParameters)
  )
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostProtocolParameters(BlockfrostProtocolParameters)
  )
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
  "blockfrost/getProtocolParameters-preprod/getProtocolParameters-preprod-7f56c39a71503c097c448dc4b47b0e34.json"

blockfrostPreviewFixture :: String
blockfrostPreviewFixture =
  "blockfrost/getProtocolParameters-preview/getProtocolParameters-preview-659470119e7874ecf112dd05db4b691d.json"

ogmiosPreprodFixture :: String
ogmiosPreprodFixture =
  "ogmios/queryLedgerState-protocolParameters-preprod-aaa9a3fbcf526a678489845a2de49600.json"

ogmiosPreviewFixture :: String
ogmiosPreviewFixture =
  "ogmios/queryLedgerState-protocolParameters-preview-256d19f15eb1f722b0b66b25d20961b8.json"

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
