module Test.Ctl.ApplyArgs (suite, main, contract) where

import Contract.Prelude

import Aeson (Aeson, encodeAeson)
import Contract.Config (ServerConfig, testnetConfig)
import Contract.Monad (Contract, launchAff_, runContract, throwContractError)
import Contract.PlutusData (PlutusData(List, Map, Bytes), toData)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Contract.Scripts (PlutusScript(PlutusScript), Validator(Validator))
import Contract.Transaction (Language, plutusV1Script)
import Ctl.Examples.AlwaysSucceeds (alwaysSucceedsScript)
import Ctl.Examples.IncludeDatum (only42Script)
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Ctl.Internal.ApplyArgs (applyArgs)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Server (stopChildProcessWithPort)
import Ctl.Internal.Plutip.Spawn (ManagedProcess, NewOutputAction(NoOp, Success), spawn)
import Ctl.Internal.QueryM (ClientError(ClientEncodingError), handleAffjaxResponse, mkHttpUrl, postAeson, scriptToAeson)
import Ctl.Internal.Serialization (toBytes) as Serialization
import Ctl.Internal.Serialization.PlutusData (convertPlutusData) as Serialization
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.BigInt (fromInt)
import Data.List.Lazy (replicate)
import Data.String (Pattern(Pattern))
import Data.String as String
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Aff (bracket)
import Foreign.Object as Object
import Mote (group, test)
import Node.ChildProcess (defaultSpawnOptions)
import Untagged.Union (asOneOf)

contract ∷ Contract () Unit
contract = liftEffect main

contractApply ∷ Array PlutusData → Contract () Validator → Contract () Unit
contractApply params loadScript = do
  Validator script <- loadScript
  let escr1 = applyArgs script params
  escr2 <- applyArgsOld script params
  applied1 <- either throwContractError pure escr1
  applied2 <- either throwContractError pure escr2
  when (not (applied1 == applied2)) $
    throwContractError "Applying script result wrong"

contractInvalidApply :: PlutusScript -> Array PlutusData -> Contract () Unit
contractInvalidApply script param = do
  case applyArgs script param of
    Left _ -> pure unit
    _ -> throwContractError "Accident"

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "Applying parameters to script" do
    traverse_
      ( test "Result of applying parameters the same for wasm and haskell"
          <<< runContract testnetConfig
      )
      (map (uncurry contractApply) $ ((\x y -> x /\ y) <$> paramss <*> scripts))
    test "Applying to invalid script returns Left" $
      runContract testnetConfig (contractInvalidApply invalidScript [ n 1 ])

  where
  invalidScript = plutusV1Script $ hexToByteArrayUnsafe "ffffff"
  scripts = [ alwaysSucceedsScript, alwaysSucceedsScriptV2, only42Script ]
  paramss =
    [ [ n 4, n 5 ]
    , [ un ]
    , [ n 7, List [ un, bytes ] ]
    , [ bytes, longBytes ]
    , [ Map [ (n 5 /\ n 7), (bytes /\ n 8) ] ]
    ]
  n k = toData (fromInt k)
  un = toData unit
  bytes = Bytes $ hexToByteArrayUnsafe "4d5f"
  longBytes = Bytes $ hexToByteArrayUnsafe $ foldl (\x y -> x <> y) "" $
    replicate 65 "4d"

main ∷ Effect Unit
main = do
  launchAff_ $
    interpret suite

-- Old haskell server implementation:

-- | Apply `PlutusData` arguments to any type isomorphic to `PlutusScript`,
-- | returning an updated script with the provided arguments applied
applyArgsOld
  :: forall (r :: Row Type)
   . PlutusScript
  -> Array PlutusData
  -> Contract r (Either ClientError PlutusScript)
applyArgsOld a = liftAff <<< applyArgsAux a

-- | Apply `PlutusData` arguments to any type isomorphic to `PlutusScript`,
-- | returning an updated script with the provided arguments applied
-- | 
-- | Spawns the ctl server that does argument application on haskell side
applyArgsAux
  :: PlutusScript
  -> Array PlutusData
  -> Aff (Either ClientError PlutusScript)
applyArgsAux script args = withCtlServer serverConfig $
  case traverse plutusDataToAeson args of
    Nothing -> pure $ Left $ ClientEncodingError "Failed to convert script args"
    Just ps -> do
      let
        language :: Language
        language = snd $ unwrap script

        url :: String
        url = mkHttpUrl serverConfig <</>> "apply-args"

        reqBody :: Aeson
        reqBody = encodeAeson
          $ Object.fromFoldable
              [ "script" /\ scriptToAeson script
              , "args" /\ encodeAeson ps
              ]
      liftAff (postAeson url reqBody)
        <#> map (PlutusScript <<< flip Tuple language)
        <<<
          handleAffjaxResponse
  where

  plutusDataToAeson :: PlutusData -> Maybe Aeson
  plutusDataToAeson =
    map
      ( encodeAeson
          <<< byteArrayToHex
          <<< Serialization.toBytes
          <<< asOneOf
      )
      <<< Serialization.convertPlutusData

  serverConfig :: ServerConfig
  serverConfig =
    { port: UInt.fromInt 8081
    , host: "localhost"
    , secure: false
    , path: Nothing
    }

  withCtlServer :: forall a. ServerConfig -> Aff a -> Aff a
  withCtlServer config a =
    bracket
      (startCtlServer config.port)
      (stopChildProcessWithPort config.port)
      $ const a

  startCtlServer :: UInt -> Aff ManagedProcess
  startCtlServer serverPort = do
    let ctlServerArgs = [ "--port", UInt.toString serverPort ]
    spawn "ctl-server" ctlServerArgs defaultSpawnOptions
      -- Wait for "CTL server starting on port" string in the output
      $ Just
      $ String.indexOf (Pattern "CTL server starting on port")
      >>> maybe NoOp (const Success)
