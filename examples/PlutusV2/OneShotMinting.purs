-- | This module demonstrates how `applyArgs` from `Contract.Scripts` can be 
-- | used to build PlutusV2 scripts with the provided arguments applied. It 
-- | creates a transaction that mints an NFT using the one-shot minting policy.
module Examples.PlutusV2.OneShotMinting
  ( contract
  , example
  , main
  ) where

import Contract.Prelude

import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Scripts (MintingPolicy)
import Contract.Test.E2E (publishTestFeedback)
import Contract.TextEnvelope (TextEnvelopeType(PlutusScriptV2))
import Contract.Transaction (TransactionInput, plutusV2Script)
import Examples.OneShotMinting
  ( mkContractWithAssertions
  , mkOneShotMintingPolicy
  )

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true

contract :: Contract () Unit
contract =
  mkContractWithAssertions "Examples.PlutusV2.OneShotMinting"
    oneShotMintingPolicyV2

foreign import oneShotMinting :: String

oneShotMintingPolicyV2 :: TransactionInput -> Contract () MintingPolicy
oneShotMintingPolicyV2 =
  mkOneShotMintingPolicy oneShotMinting PlutusScriptV2 plutusV2Script

