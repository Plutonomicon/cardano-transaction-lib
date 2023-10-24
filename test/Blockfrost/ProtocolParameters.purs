module Test.Ctl.Blockfrost.ProtocolParameters (main, suite) where

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

blockfrostFixture :: String
blockfrostFixture =
  "blockfrost/getProtocolParameters/getProtocolParameters-7fe834fd628aa322eedeb3d8c7c1dd61.json"

ogmiosFixture :: String
ogmiosFixture =
  "ogmios/queryLedgerState-protocolParameters-13365782430bcc1da07918d41b57dd73.json"

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
  test "ProtocolParameter parsing verification" do
    BlockfrostProtocolParameters blockfrostFixture' <- loadFixture
      blockfrostFixture
    OgmiosProtocolParameters ogmiosFixture' <- loadFixture ogmiosFixture

    blockfrostFixture' `shouldEqual` ogmiosFixture'
