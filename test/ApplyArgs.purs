module Test.ApplyArgs (main) where

import Contract.Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (Contract, launchAff_, runContract, throwContractError)
import Contract.PlutusData (PlutusData(..), toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Scripts (PlutusScript, Validator(..))
import Contract.Scripts as Scripts
import Contract.Transaction (plutusV1Script)
import Ctl.Examples.AlwaysSucceeds (alwaysSucceedsScript)
import Ctl.Examples.IncludeDatum (only42Script)
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Ctl.Internal.ApplyArgs (applyArgsWithErrors)
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.Array (zipWith)
import Data.BigInt (fromInt)
import Mote (group, test)

contractApply ∷ Array PlutusData → Contract () Validator → Contract () Unit
contractApply params loadScript = do 
  Validator script <- loadScript
  log $ "Starting with " <> show script <> show params
  log $ show script
  eapplied1 <- Scripts.applyArgs script params
  log $ show eapplied1
  let mapplied2 = applyArgsWithErrors script params
  log $ show mapplied2
  applied1 <- either throwContractError pure eapplied1
  applied2 <- either (throwContractError) pure mapplied2
  if applied1 == applied2 then
      log "Alright, scripts match"
  else 
      throwContractError "Scripts applied but different results"

contractInvalidApply :: PlutusScript -> Array PlutusData -> Contract () Unit
contractInvalidApply script param  = do 
  case applyArgsWithErrors script param of 
    Left _ -> pure unit
    _ -> throwContractError "Accident"

suite :: TestPlanM (Aff Unit) Unit
suite = 
  group "Applying parameters to script" do
    traverse_ (
      test "Result of applying parameters the same for wasm and haskell" 
      <<< runContract testnetConfig)
      (map (uncurry contractApply) $ ((\x y -> x /\ y) <$> paramss <*> scripts))
    test "Applying to invalid script returns Left" $ 
      runContract testnetConfig (contractInvalidApply invalidScript [n 1])

  where
    invalidScript = plutusV1Script $ hexToByteArrayUnsafe "ffffff" 
    scripts = [alwaysSucceedsScript, alwaysSucceedsScriptV2, only42Script]
    paramss = [
      [n 4, n 5],
      [un],
      [n 7, List [un, bytes]],
      [bytes],
      [Map [(n 5 /\ n 7), (bytes /\ n 8)]]
    ]
    n k = toData (fromInt k)
    un = toData unit
    bytes = Bytes $ hexToByteArrayUnsafe "4d5f" 

main ∷ Effect Unit
main = do 
  launchAff_ $ 
    interpret suite
