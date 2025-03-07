module Test.Ctl.ProtocolParameters
  ( main
  , suite
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeJsonString)
import Cardano.Blockfrost.BlockfrostProtocolParameters
  ( BlockfrostProtocolParameters(BlockfrostProtocolParameters)
  )
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Control.Monad.Error.Class (liftEither)
import Ctl.Internal.QueryM.Ogmios.Types
  ( OgmiosProtocolParameters(OgmiosProtocolParameters)
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
  "blockfrost/getProtocolParameters-preprod/getProtocolParameters-preprod-18063c9b925b4ac71a45459a61b9121c.json"

blockfrostPreviewFixture :: String
blockfrostPreviewFixture =
  "blockfrost/getProtocolParameters-preview/getProtocolParameters-preview-80b669933a38e1e1bab58b43b4993836.json"

ogmiosPreprodFixture :: String
ogmiosPreprodFixture =
  "ogmios/queryLedgerState-protocolParameters-preprod-45142c2c7698b34c97bf8d6fdf344129.json"

ogmiosPreviewFixture :: String
ogmiosPreviewFixture =
  "ogmios/queryLedgerState-protocolParameters-preview-48914f60e567c5b17f457ecdc5135223.json"

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
