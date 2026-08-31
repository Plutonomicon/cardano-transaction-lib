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
  "blockfrost/getProtocolParameters-preprod/getProtocolParameters-preprod-69e4f7fdd8b088e4de653fc6f2a57587.json"

blockfrostPreviewFixture :: String
blockfrostPreviewFixture =
  "blockfrost/getProtocolParameters-preview/getProtocolParameters-preview-5bc80dc21804f41760e0620f964f5b45.json"

ogmiosPreprodFixture :: String
ogmiosPreprodFixture =
  "ogmios/queryLedgerState-protocolParameters-preprod-b1f489b5d2c4a04f8513dd9b8718cd3f.json"

ogmiosPreviewFixture :: String
ogmiosPreviewFixture =
  "ogmios/queryLedgerState-protocolParameters-preview-b56479534c05c935ed0018ddd188c714.json"

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
