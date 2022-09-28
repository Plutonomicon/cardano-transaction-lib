-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address
module Ctl.Examples.Cip30
  (main
  ) where

import Contract.Prelude

import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Wallet (getChangeAddress, getNetworkId, getRewardAddresses, getUnusedAddresses)


main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.Cip30Test"
  performAndLog "getNetworkId" getNetworkId
  performAndLog "getUnusedAddresses" getUnusedAddresses
  performAndLog "getChangeAddress" getChangeAddress
  performAndLog "getRewardAddresses" getRewardAddresses

performAndLog :: forall (a::Type) . Show a => String -> 
  Contract () a -> Contract () Unit 
performAndLog msg cont = do 
  result <- cont
  logInfo' $ msg <> ": " <> show result

