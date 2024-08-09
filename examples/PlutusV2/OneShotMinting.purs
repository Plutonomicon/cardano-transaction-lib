-- | This module demonstrates how `applyArgs` from `Cardano.Plutus.ApplyArgs`
-- | (from https://github.com/mlabs-haskell/purescript-uplc-apply-args) can be
-- | used to build PlutusV2 scripts with the provided arguments applied. It
-- | creates a transaction that mints an NFT using the one-shot minting policy.
module Ctl.Examples.PlutusV2.OneShotMinting
  ( contract
  , example
  , main
  , oneShotMintingPolicyScriptV2
  ) where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , KnownWallet(Nami)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Monad (Contract, launchAff_, liftContractE, runContract)
import Contract.Scripts (PlutusScript)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (TransactionInput)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.OneShotMinting
  ( mkContractWithAssertions
  , mkOneShotMintingPolicy
  )
import Effect.Exception (error)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Nami) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

contract :: Contract Unit
contract =
  mkContractWithAssertions "Examples.PlutusV2.OneShotMinting"
    oneShotMintingPolicyScriptV2

oneShotMintingPolicyScriptV2 :: TransactionInput -> Contract PlutusScript
oneShotMintingPolicyScriptV2 txInput = do
  script <- liftMaybe (error "Error decoding oneShotMinting") do
    envelope <- decodeTextEnvelope oneShotMinting
    plutusScriptFromEnvelope envelope
  liftContractE $
    mkOneShotMintingPolicy script txInput

foreign import oneShotMinting :: String
