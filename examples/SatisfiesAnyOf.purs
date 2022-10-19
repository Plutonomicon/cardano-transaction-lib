-- | This module creates a transaction that tries to pay
-- | 2 Ada to the `AlwaysSucceeds` script address by
-- | providing an incorrect DatumHash in the first constraints
-- | list in `mustSatisfyAnyOf`. The transaction then
-- | goes through with the second constraints list.
module Ctl.Examples.SatisfiesAnyOf
  ( example
  , main
  , testMustSatisfyAnyOf
  ) where

import Contract.Prelude

import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData(Integer)
  , unitDatum
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Transaction (awaitTxConfirmed)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.AlwaysSucceeds (alwaysSucceedsScript) as AlwaysSucceeds
import Ctl.Examples.Helpers (buildBalanceSignAndSubmitTx) as Helpers
import Ctl.Internal.Hashing (datumHash) as Hashing
import Data.BigInt as BigInt
import Effect.Exception (error)

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg do
    logInfo' "Running Examples.SatisfiesAnyOf"
    validator <- AlwaysSucceeds.alwaysSucceedsScript
    let vhash = validatorHash validator
    logInfo' "Attempt to lock value"
    testMustSatisfyAnyOf vhash
  publishTestFeedback true

wrongDatum :: Datum
wrongDatum = Datum $ Integer $ BigInt.fromInt 42

testMustSatisfyAnyOf :: ValidatorHash -> Contract () Unit
testMustSatisfyAnyOf vhash = do
  wrongDatumHash <- liftMaybe (error "Cannot get DatumHash") $ Hashing.datumHash
    wrongDatum
  correctDatumHash <- liftMaybe (error "Cannot get DatumHash") $
    Hashing.datumHash unitDatum
  let
    payToConstraint :: TxConstraints Unit Unit
    payToConstraint =
      Constraints.mustPayToScript vhash unitDatum
        Constraints.DatumWitness
        $ Value.lovelaceValueOf
        $ BigInt.fromInt 2_000_000

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSatisfyAnyOf
      [ payToConstraint <> Constraints.mustHashDatum wrongDatumHash unitDatum
      , payToConstraint <> Constraints.mustHashDatum correctDatumHash unitDatum
      ]

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  lockTxId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints
  awaitTxConfirmed lockTxId
  logInfo' "Successfully locked value"
