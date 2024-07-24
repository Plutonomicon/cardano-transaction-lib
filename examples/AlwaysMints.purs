-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a value using the `AlwaysMints` policy
module Ctl.Examples.AlwaysMints
  ( alwaysMintsPolicy
  , alwaysMintsPolicyMaybe
  , contract
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.Types (PlutusScript)
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Config
  ( ContractParams
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.ScriptLookups as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Ctl.Examples.Helpers (mkAssetName) as Helpers

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec = Just $ ConnectToGenericCip30 "nami" { cip95: false }
  }

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.AlwaysMints"
  mp <- alwaysMintsPolicy
  let cs = PlutusScript.hash mp
  tn <- Helpers.mkAssetName "TheToken"
  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustMintValue
      $ Mint.singleton cs tn
      $ Int.fromInt 100

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.plutusMintingPolicy mp

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"

example :: ContractParams -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract

foreign import alwaysMints :: String

alwaysMintsPolicyMaybe :: Maybe PlutusScript
alwaysMintsPolicyMaybe =
  plutusScriptFromEnvelope =<< decodeTextEnvelope alwaysMints

alwaysMintsPolicy :: Contract PlutusScript
alwaysMintsPolicy =
  liftContractM "Error decoding alwaysMintsPolicy" alwaysMintsPolicyMaybe
