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

import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Ctl.Examples.Helpers (mkAssetName, mkCurrencySymbol) as Helpers

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.AlwaysMints"
  mp /\ cs <- Helpers.mkCurrencySymbol alwaysMintsPolicy
  tn <- Helpers.mkAssetName "TheToken"
  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustMintValue
      $ Mint.singleton cs tn
      $ Int.fromInt 100

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.mintingPolicy mp

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"

example :: ContractParams -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract

foreign import alwaysMints :: String

alwaysMintsPolicyMaybe :: Maybe MintingPolicy
alwaysMintsPolicyMaybe = do
  PlutusMintingPolicy <$>
    (plutusScriptFromEnvelope =<< decodeTextEnvelope alwaysMints)

alwaysMintsPolicy :: Contract MintingPolicy
alwaysMintsPolicy =
  liftContractM "Error decoding alwaysMintsPolicy" alwaysMintsPolicyMaybe
