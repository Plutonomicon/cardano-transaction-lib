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
  "blockfrost/getProtocolParameters/getProtocolParameters-3457670a74fb5f9895d5cce9b6826c85.json"

ogmiosFixture :: String
ogmiosFixture =
  "ogmios/queryLedgerState-protocolParameters-f39db65d5d9dc11e511f061b37349173.json"

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
    { result: OgmiosProtocolParameters ogmiosFixture' }
      :: { result :: OgmiosProtocolParameters } <- loadFixture ogmiosFixture

    blockfrostFixture' `shouldEqual` ogmiosFixture'
