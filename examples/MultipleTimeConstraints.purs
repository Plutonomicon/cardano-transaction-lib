-- | This module test the use of multiple time constraints
module Ctl.Examples.MultipleTimeConstraints (main, contract, example) where

import Contract.Prelude

import Contract.Chain (currentTime)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedE, runContract)
import Contract.ScriptLookups (UnattachedUnbalancedTx)
import Contract.ScriptLookups as Lookups
import Contract.Test.E2E (publishTestFeedback)
import Contract.Time
  ( Slot
  , mkFiniteInterval
  )
import Contract.TxConstraints as Constraints
import Data.BigInt as BigInt

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.MultipleTimeConstraints"
  now <- currentTime
  let
    timeRange1 =
      mkFiniteInterval
        now
        $ wrap (unwrap now + BigInt.fromInt 3600000)
    timeRange2 =
      mkFiniteInterval
        (wrap (unwrap now + BigInt.fromInt 8000000))
        (wrap (unwrap now + BigInt.fromInt 800000))

    constraints1 :: Constraints.TxConstraints Void Void
    constraints1 = Constraints.mustValidateIn timeRange1

    constraints2 :: Constraints.TxConstraints Void Void
    constraints2 = Constraints.mustValidateIn timeRange2

    constraints3 :: Constraints.TxConstraints Void Void
    constraints3 = constraints1 <> constraints2

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  unbalancedTx1 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints1
  unbalancedTx2 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints2
  unbalancedError <- Lookups.mkUnbalancedTx lookups constraints3

  logInfo' "unbalanced1"
  logInfo' $ show $ getTimes unbalancedTx1
  logInfo' "unbalanced2"
  logInfo' $ show $ getTimes unbalancedTx2
  logInfo' "unbalanced3"
  logInfo' $ show unbalancedError
  liftAff $ publishTestFeedback true
  where
  getTimes :: UnattachedUnbalancedTx -> Maybe Slot /\ Maybe Slot
  getTimes utx =
    let
      body = unwrap
        (((unwrap (unwrap ((unwrap utx).unbalancedTx)).transaction)).body)
    in
      body.validityStartInterval /\ body.ttl

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
