module Test.Ctl.Blockfrost.GenerateFixtures.NativeScript (main) where

import Contract.Prelude

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
import Contract.Scripts (NativeScript, ScriptHash)
import Contract.Transaction
  ( ScriptRef(NativeScriptRef)
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (TxConstraints) as Constraints
import Contract.Value (lovelaceValueOf) as Value
import Contract.Wallet (ownPaymentPubKeyHash, ownStakePubKeyHash)
import Ctl.Examples.Helpers (mustPayToPubKeyStakeAddressWithScriptRef)
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostEndpoint(NativeScriptByHash)
  , BlockfrostRawResponse
  , runBlockfrostServiceTestM
  )
import Ctl.Internal.Service.Blockfrost (getScriptByHash) as Blockfrost
import Data.Array (mapWithIndex)
import Data.BigInt (fromInt) as BigInt
import Data.UInt (fromInt) as UInt
import Test.Ctl.Blockfrost.GenerateFixtures.Helpers
  ( blockfrostBackend
  , getSkeyFilepathFromEnv
  , storeBlockfrostFixture
  )
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (randomSample')
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main =
  contractParams >>=
    (launchAff_ <<< flip runContract (generateFixtures 10))
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
          Just $ UseKeys (PrivatePaymentKeyFile skeyFilepath) Nothing
      }

generateFixtures :: Int -> Contract Unit
generateFixtures numFixtures = do
  -- TODO: Remove this line and use Blockfrost as the default backend instead.
  backend <- liftEffect blockfrostBackend

  nativeScripts <- liftEffect $ randomSample' numFixtures arbitrary
  traverse_ (generateFixtureForScript backend) (mapWithIndex (/\) nativeScripts)
  where
  generateFixtureForScript
    :: BlockfrostBackend -> Int /\ NativeScript -> Contract Unit
  generateFixtureForScript backend (i /\ nativeScript) = do
    pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
    skh <- ownStakePubKeyHash
    let
      constraints :: Constraints.TxConstraints Void Void
      constraints =
        mustPayToPubKeyStakeAddressWithScriptRef pkh skh nativeScriptRef
          (Value.lovelaceValueOf $ BigInt.fromInt 2_000_000)

      lookups :: Lookups.ScriptLookups Void
      lookups = mempty

    txHash <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txHash

    -- TODO:
    -- backend <- liftedM "Failed to get Blockfrost backend"
    --   (getBlockfrostBackend <$> asks _.backend)

    eiNativeScript <- liftAff $
      runBlockfrostServiceTestM (\_ -> pure unit) backend
        (Just onBlockfrostRawResponse)
        Nothing
        (Blockfrost.getScriptByHash nativeScriptHash)

    eiNativeScript `shouldEqual` Right (Just nativeScriptRef)
    where
    nativeScriptRef :: ScriptRef
    nativeScriptRef = NativeScriptRef nativeScript

    nativeScriptHash :: ScriptHash
    nativeScriptHash = Hashing.scriptRefHash nativeScriptRef

    onBlockfrostRawResponse
      :: BlockfrostEndpoint -> BlockfrostRawResponse -> Aff Unit
    onBlockfrostRawResponse query rawResponse =
      case query of
        NativeScriptByHash h | h == nativeScriptHash ->
          storeBlockfrostFixture i "getNativeScriptByHash" rawResponse
        _ -> pure unit
