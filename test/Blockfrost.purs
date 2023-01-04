module Test.Ctl.Blockfrost (main, testPlan) where

import Prelude

import Contract.Config (blockfrostPublicPreviewServerConfig)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Transaction (DataHash)
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.Helpers (liftedM)
import Ctl.Internal.Serialization.Hash (ScriptHash, scriptHashFromBytes)
import Ctl.Internal.Service.Blockfrost
  ( getDatumByHash
  , getScriptByHash
  , runBlockfrostServiceM
  )
import Data.Array ((!!))
import Data.Either (fromRight, isRight)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Mote (group, test)
import Node.Process (argv)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Runner (defaultConfig)

-- Run with `spago test --main Test.Ctl.Blockfrost --exec-args PREVIEW_API_KEY`
main :: Effect Unit
main = do
  apiKey <- liftedM (error "ApiKey not supplied") $ (_ !! 1) <$> argv
  launchAff_ do
    interpretWithConfig
      defaultConfig { exit = true }
      ( testPlan
          { blockfrostConfig: blockfrostPublicPreviewServerConfig
          , blockfrostApiKey: Just apiKey
          }
      )

testPlan :: BlockfrostBackend -> TestPlanM (Aff Unit) Unit
testPlan backend = group "Blockfrost" do
  let
    mkDatumHash :: String -> DataHash
    mkDatumHash = wrap <<< hexToByteArrayUnsafe

    mkStringHash :: String -> ScriptHash
    mkStringHash s = unsafePartial $ fromJust $ scriptHashFromBytes $
      hexToByteArrayUnsafe s

  test "getDatumByHash - not found" do
    runBlockfrostServiceM backend do
      datum <- getDatumByHash $ mkDatumHash
        "e1457a0c47dfb7a2f6b8fbb059bdceab163c05d34f195b87b9f2b30e"
      datum `shouldSatisfy` isRight
      let result = fromRight Nothing datum
      result `shouldEqual` Nothing

  test "getScriptByHash - not found" do
    runBlockfrostServiceM backend do
      script <- getScriptByHash $ mkStringHash
        "e1457a0c47dfb7a2f6b8fbb059bdceab163c05d34f195b87b9f2b30e"
      script `shouldSatisfy` isRight
      let result = fromRight Nothing script
      result `shouldEqual` Nothing
