module Test.Ctl.Blockfrost.GenerateFixtures.GetNativeScriptByHash (main) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash, ownStakePubKeyHash)
import Contract.Config
  ( ContractParams
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , WalletSpec(UseKeys)
  , blockfrostPublicPreviewServerConfig
  , testnetConfig
  )
import Contract.Hashing (scriptRefHash) as Hashing
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractM
  , liftedM
  , runContract
  )
import Contract.ScriptLookups (ScriptLookups) as Lookups
import Contract.Scripts (NativeScript, ScriptHash)
import Contract.Transaction
  ( ScriptRef(NativeScriptRef)
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (TxConstraints) as Constraints
import Contract.Value (lovelaceValueOf) as Value
import Ctl.Examples.Helpers (mustPayToPubKeyStakeAddressWithScriptRef)
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostEndpoint(GetNativeScriptByHash)
  , runBlockfrostServiceTestM
  )
import Ctl.Internal.Service.Blockfrost (getScriptByHash) as Blockfrost
import Data.Array (mapWithIndex)
import Data.BigInt (fromInt) as BigInt
import Data.Map (lookup) as Map
import Effect.Exception (throw)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (writeTextFile)
import Node.Path (concat)
import Node.Process (lookupEnv)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (randomSample')
import Test.Spec.Assertions (shouldEqual)

foreign import md5 :: String -> String

main :: Effect Unit
main =
  contractParams >>=
    (launchAff_ <<< flip runContract (generateFixtures 10))
  where
  contractParams :: Effect ContractParams
  contractParams = do
    -- blockfrostApiKey <- lookupEnv' "BLOCKFROST_API_KEY"
    skeyFilepath <- lookupEnv' "SKEY_FILEPATH"
    pure $ testnetConfig
      -- TODO: Configure Contract with Blockfrost as the default backend.
      -- { backendParams =
      --    mkBlockfrostBackendParams
      --      { blockfrostConfig: blockfrostPublicPreviewServerConfig
      --      , blockfrostApiKey: Just blockfrostApiKey
      --      }
      { logLevel = Info
      , walletSpec =
          Just $ UseKeys (PrivatePaymentKeyFile skeyFilepath) Nothing
      }

lookupEnv' :: String -> Effect String
lookupEnv' var =
  lookupEnv var >>=
    maybe (throw $ var <> " environment variable not set") pure

generateFixtures :: Int -> Contract Unit
generateFixtures numFixtures = do
  -- TODO: Remove this code and use Blockfrost as the default backend instead.
  blockfrostApiKey <- liftEffect $ lookupEnv' "BLOCKFROST_API_KEY"
  let
    backend :: BlockfrostBackend
    backend =
      { blockfrostConfig: blockfrostPublicPreviewServerConfig
      , blockfrostApiKey: Just blockfrostApiKey
      }
  --

  nativeScripts <- liftEffect $ randomSample' numFixtures arbitrary
  traverse_ (generateFixtureForScript backend) (mapWithIndex (/\) nativeScripts)
  where
  generateFixtureForScript
    :: BlockfrostBackend -> Int /\ NativeScript -> Contract Unit
  generateFixtureForScript backend (i /\ nativeScript) = do
    let
      nativeScriptRef :: ScriptRef
      nativeScriptRef = NativeScriptRef nativeScript

      nativeScriptHash :: ScriptHash
      nativeScriptHash = Hashing.scriptRefHash nativeScriptRef

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

    eiNativeScript /\ rawResponses <-
      liftAff $ runBlockfrostServiceTestM backend
        (Blockfrost.getScriptByHash nativeScriptHash)

    eiNativeScript `shouldEqual` Right (Just nativeScriptRef)

    rawResponse <- liftContractM "Could not find raw response" $
      Map.lookup (GetNativeScriptByHash nativeScriptHash) rawResponses

    liftAff $ storeFixture rawResponse
    where
    storeFixture :: String -> Aff Unit
    storeFixture resp =
      let
        respHash = md5 resp
        query = "getNativeScriptByHash"
        filename = query <> "-" <> respHash <> ".json"
        fp = concat [ "fixtures", "test", "blockfrost", query, filename ]
      in
        writeTextFile UTF8 fp resp
          *> log ("Successfully stored fixture #" <> show i <> " to: " <> fp)
