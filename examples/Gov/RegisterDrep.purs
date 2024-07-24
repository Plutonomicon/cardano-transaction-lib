module Ctl.Examples.Gov.RegisterDrep
  ( contract
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.Types.Credential (Credential(PubKeyHashCredential))
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints (mustRegisterDrep) as Constraints
import Contract.Wallet (ownDrepPubKeyHash)

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.Gov.RegisterDrep"
  drepCred <- PubKeyHashCredential <$> ownDrepPubKeyHash
  let
    constraints :: TxConstraints
    constraints = Constraints.mustRegisterDrep drepCred Nothing

  txHash <- submitTxFromConstraints mempty constraints
  awaitTxConfirmed txHash
  logInfo' "Tx submitted successfully!"
