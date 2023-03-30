-- | This example demonstrates how storage setup affect concurrent spending.
module Ctl.Examples.ConcurrentSpending where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeysHashes, ownStakePubKeysHashes)
import Contract.Chain (waitNSlots)
import Contract.Config (ContractParams, mkRefStorage, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, ContractEnv, launchAff_, liftedM, runContract)
import Contract.Numeric.Natural as Natural
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( awaitTxConfirmedWithTimeout
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Control.Monad.Error.Class (try)
import Control.Monad.Reader (ask, local)
import Control.Parallel (parSequence)
import Data.Array (head, range)
import Data.BigInt as BigInt
import Effect.Exception (throw)

main :: Effect Unit
main = example testnetNamiConfig

onePkh2PkhTx :: Contract Unit
onePkh2PkhTx = do
  logInfo' "Running Examples.ConcurrentSpending"
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeysHashes
  skh <- liftedM "Failed to get own SKH" $ join <<< head <$>
    ownStakePubKeysHashes

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToPubKeyAddress pkh skh
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmedWithTimeout (wrap 100.0) txId
  logInfo' $ "Tx submitted successfully!"

disconnect
  :: forall a
   . (ContractEnv -> Effect ContractEnv)
  -> Array (Contract a)
  -> Contract Unit
disconnect f contracts = do
  void $ parSequence $ contracts <#> \c -> do
    env <- ask
    env' <- liftEffect $ f env
    local (const env') c

-- this test is flaky, but the probability of failing is low
contract :: Contract Unit
contract = do
  -- error will probably happen due to concurrent spending at least once
  eiResult <- try $ for (range 0 10) \_ -> do
    disconnect resetStorage [ onePkh2PkhTx, onePkh2PkhTx, onePkh2PkhTx ]
      <* waitNSlots (Natural.fromInt' 20)
  logInfo' $ show eiResult
  when (isRight eiResult) do
    liftEffect $ throw "Expected error"
  void $ waitNSlots (Natural.fromInt' 20)
  -- no error must happen
  disconnect pure [ onePkh2PkhTx, onePkh2PkhTx, onePkh2PkhTx ]

resetStorage :: ContractEnv -> Effect ContractEnv
resetStorage env = do
  storage <- mkRefStorage
  pure $ env { storage = storage }

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
