module Test.ApplyArgs (main) where

import Contract.Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (Contract, launchAff_, runContract, throwContractError)
import Contract.PlutusData (deserializeData, toData)
import Contract.Scripts (PlutusScript(..), Validator(..))
import Contract.Scripts as Scripts
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV1FromEnvelope)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Ctl.Examples.AlwaysSucceeds (alwaysSucceeds, alwaysSucceedsScript)
import Ctl.Examples.IncludeDatum (only42Script)
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Ctl.Internal.ApplyArgs (applyArgs)
import Ctl.Internal.QueryM.Config (testnetQueryConfig)
import Ctl.Internal.Serialization.Types (PlutusData)
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Ctl.Internal.Types.BigNum (toBigInt)
import Ctl.Internal.Types.CborBytes (CborBytes(..))
import Data.BigInt (fromInt)
import Data.Int (toNumber)
import Effect.Console (logShow)
import Effect.Exception (error, throwException)
import Mote (group, test)

-- f ∷ PlutusScript → Maybe 
-- f sc = deserializeData $ CborBytes (case sc of PlutusScript (ba /\ _) -> ba)

contract ∷ Contract () Validator → Contract () Unit
contract loadScript = do 
  log "Starting"
  let params = [toData unit,toData (fromInt 5)]
  Validator script <- loadScript
  log $ show script
  eapplied1 <- Scripts.applyArgs script params
  log $ show eapplied1
  let mapplied2 = applyArgs script params
  log $ show mapplied2
  applied1 <- either throwContractError pure eapplied1
  applied2 <- maybe (throwContractError "Error applying args with wasm") pure mapplied2
  if applied1 == applied2 then
      log "Alright, scripts match"
  else 
      throwContractError "Scripts applied but different results"

scripts = [alwaysSucceedsScript, alwaysSucceedsScriptV2, only42Script]

suite :: TestPlanM (Aff Unit) Unit
suite = 
  group "Logging" do
    traverse_ (
      test "Logs that are not suppressed really appear" 
      <<< runContract testnetConfig 
      <<< contract) 
      scripts

main ∷ Effect Unit
main = do 
  log "Hello"
  launchAff_ $ 
    interpret suite

-- main :: Effect Unit
-- main = do
--     logShow $ strange (toNumber 5) 
--     log "hello worlds1"
--     _ <- throwException (error "helloworlds exc")
--     log "hello worlds"