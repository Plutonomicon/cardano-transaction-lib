-- | This module demonstrates how `applyArgs` from `Contract.Scripts` can be
-- | used to build PlutusV2 scripts with the provided arguments applied. It
-- | creates a transaction that mints an NFT using the one-shot minting policy.
module Ctl.Examples.PlutusV2.OneShotMinting
  ( contract
  , example
  , main
  , oneShotMintingPolicyScriptV2
  , oneShotMintingPolicyV2
  ) where

import Contract.Prelude

import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractE
  , runContract
  )
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (TransactionInput)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.OneShotMinting
  ( mkContractWithAssertions
  , mkOneShotMintingPolicy
  )
import Effect.Exception (error)

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

contract :: Contract Unit
contract =
  mkContractWithAssertions "Examples.PlutusV2.OneShotMinting"
    oneShotMintingPolicyV2

oneShotMintingPolicyV2 :: TransactionInput -> Contract MintingPolicy
oneShotMintingPolicyV2 =
  map PlutusMintingPolicy <<< oneShotMintingPolicyScriptV2

oneShotMintingPolicyScriptV2 :: TransactionInput -> Contract PlutusScript
oneShotMintingPolicyScriptV2 txInput = do
  script <- liftMaybe (error "Error decoding oneShotMinting") do
    envelope <- decodeTextEnvelope oneShotMinting
    plutusScriptV2FromEnvelope envelope
  liftContractE $
    mkOneShotMintingPolicy script txInput

foreign import oneShotMinting :: String
