-- | This module creates an unbalanced transaction
-- | with the `mustSatisfyAnyOf` constraint in which
-- | the evaluation of the first constraint list throws
-- | a catched error and the evaluation of the second list succeeds.
module Ctl.Examples.SatisfiesAnyOf
  ( example
  , main
  , testMustSatisfyAnyOf
  ) where

import Contract.Prelude

import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Hashing (datumHash) as Hashing
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData(Integer)
  , unitDatum
  )
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import JS.BigInt as BigInt

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg do
    logInfo' "Running Examples.SatisfiesAnyOf"
    testMustSatisfyAnyOf

wrongDatum :: Datum
wrongDatum = Datum $ Integer $ BigInt.fromInt 42

testMustSatisfyAnyOf :: Contract Unit
testMustSatisfyAnyOf = do
  let
    wrongDatumHash = Hashing.datumHash wrongDatum
    correctDatumHash = Hashing.datumHash unitDatum

    constraints :: TxConstraints
    constraints = Constraints.mustSatisfyAnyOf
      [ Constraints.mustHashDatum wrongDatumHash unitDatum
      , Constraints.mustHashDatum correctDatumHash unitDatum
      ]

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  void $ mkUnbalancedTx lookups constraints
