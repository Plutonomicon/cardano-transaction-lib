module Test.Ctl.Blockfrost.GenerateFixtures.ScriptInfo (main) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Config
  ( ContractParams
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , WalletSpec(UseKeys)
  , defaultKupoServerConfig
  , defaultOgmiosWsConfig
  , mkCtlBackendParams
  , testnetConfig
  )
import Contract.Hashing (scriptRefHash) as Hashing
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.ScriptLookups (ScriptLookups) as Lookups
import Contract.Scripts (ScriptHash)
import Contract.Transaction
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (TxConstraints) as Constraints
import Contract.Value (Value)
import Contract.Value (lovelaceValueOf) as Value
import Contract.Wallet (ownPaymentPubKeyHash, ownStakePubKeyHash)
import Ctl.Examples.AlwaysSucceeds (alwaysSucceedsScript)
import Ctl.Examples.Helpers (mustPayToPubKeyStakeAddressWithScriptRef)
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostEndpoint(ScriptInfo)
  , BlockfrostRawResponse
  , BlockfrostScriptLanguage(NativeScript, PlutusV1Script, PlutusV2Script)
  , runBlockfrostServiceTestM
  )
import Ctl.Internal.Service.Blockfrost (getScriptInfo) as Blockfrost
import Data.Array (zip) as Array
import Data.FoldableWithIndex (forWithIndex_)
import Data.UInt (fromInt) as UInt
import Test.Ctl.Blockfrost.GenerateFixtures.Helpers
  ( blockfrostBackend
  , getSkeyFilepathFromEnv
  , storeBlockfrostFixture
  )
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main =
  contractParams >>=
    (launchAff_ <<< flip runContract generateFixtures)
  where
  contractParams :: Effect ContractParams
  contractParams = do
    -- blockfrostApiKey <- lookupEnv' "BLOCKFROST_API_KEY"
    skeyFilepath <- getSkeyFilepathFromEnv
    pure $ testnetConfig
      -- TODO: Configure Contract with Blockfrost as the default backend.
      -- { backendParams =
      --    mkBlockfrostBackendParams
      --      { blockfrostConfig: blockfrostPublicPreviewServerConfig
      --      , blockfrostApiKey: Just blockfrostApiKey
      --      }
      { backendParams =
          mkCtlBackendParams
            { ogmiosConfig: defaultOgmiosWsConfig
            , kupoConfig:
                defaultKupoServerConfig
                  { port = UInt.fromInt 1442, path = Nothing }
            }
      , logLevel = Info
      , walletSpec =
          Just $ UseKeys (PrivatePaymentKeyFile skeyFilepath) Nothing Nothing
      }

generateFixtures :: Contract Unit
generateFixtures = do
  -- TODO: Remove this line and use Blockfrost as the default backend instead.
  backend <- liftEffect blockfrostBackend

  nativeScriptRef <- liftEffect (NativeScriptRef <$> randomSampleOne arbitrary)
  v1PlutusScriptRef <- PlutusScriptRef <$> alwaysSucceedsScript
  v2PlutusScriptRef <- PlutusScriptRef <$> alwaysSucceedsScriptV2

  let
    scriptRefs = [ nativeScriptRef, v1PlutusScriptRef, v2PlutusScriptRef ]
    scriptLanguages = [ NativeScript, PlutusV1Script, PlutusV2Script ]
    scriptHashes = Hashing.scriptRefHash <$> scriptRefs
    hashesLanguages = Array.zip scriptHashes scriptLanguages

  pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  skh <- ownStakePubKeyHash
  let
    value :: Value
    value = Value.lovelaceValueOf $ BigNum.fromInt 2_000_000

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat $ scriptRefs <#>
        flip (mustPayToPubKeyStakeAddressWithScriptRef pkh skh) value

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  txHash <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txHash

  forWithIndex_ hashesLanguages \i (scriptHash /\ language) -> do
    scriptInfo <- liftAff $
      runBlockfrostServiceTestM (\_ -> pure unit) backend
        (Just $ onBlockfrostRawResponse i scriptHash)
        Nothing
        (Blockfrost.getScriptInfo scriptHash)
    scriptInfo `shouldEqual` Right (Just $ wrap { language })
  where
  onBlockfrostRawResponse
    :: Int
    -> ScriptHash
    -> BlockfrostEndpoint
    -> BlockfrostRawResponse
    -> Aff Unit
  onBlockfrostRawResponse i scriptHash query rawResponse =
    case query of
      ScriptInfo h | h == scriptHash ->
        storeBlockfrostFixture i "getScriptInfo" rawResponse
      _ -> pure unit
