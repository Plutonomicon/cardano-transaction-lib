module Ctl.Examples.ChangeGeneration (checkChangeOutputsDistribution) where

import Prelude

import Contract.BalanceTxConstraints (mustSendChangeWithDatum)
import Contract.Monad (Contract)
import Contract.PlutusData
  ( Datum(Datum)
  , OutputDatum(OutputDatum)
  , PlutusData(Integer)
  , unitDatum
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( _body
  , _outputs
  , awaitTxConfirmed
  , balanceTxWithConstraints
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value as Value
import Contract.Wallet (ownPaymentPubKeyHashes, ownStakePubKeyHashes)
import Ctl.Examples.AlwaysSucceeds as AlwaysSucceeds
import Data.Array (fold, length, replicate, take, zip)
import Data.Lens (to, (^.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import JS.BigInt (fromInt) as BigInt
import Test.Spec.Assertions (shouldEqual)

-- | A contract that creates `outputsToScript` number of outputs at a script address,
-- | `outputsToSelf` outputs going to own address, and asserts that the number of change
-- | outputs is equal to `expectedOutputs`.
checkChangeOutputsDistribution :: Int -> Int -> Int -> Contract Unit
checkChangeOutputsDistribution outputsToScript outputsToSelf expectedOutputs =
  do
    pkhs <- ownPaymentPubKeyHashes
    skhs <- ownStakePubKeyHashes
    validator <- AlwaysSucceeds.alwaysSucceedsScript
    let
      vhash = validatorHash validator
      value = Value.lovelaceValueOf $ BigInt.fromInt 1000001

      constraintsToSelf :: TxConstraints
      constraintsToSelf = fold <<< take outputsToSelf <<< fold
        $ replicate outputsToSelf
        $ zip pkhs skhs <#> \(Tuple pkh mbSkh) -> case mbSkh of
            Nothing -> Constraints.mustPayToPubKey pkh value
            Just skh -> Constraints.mustPayToPubKeyAddress pkh skh value

      constraintsToScripts :: TxConstraints
      constraintsToScripts = fold $ replicate outputsToScript
        $ Constraints.mustPayToScript vhash unitDatum
            Constraints.DatumWitness
            value

      constraints = constraintsToSelf <> constraintsToScripts

      lookups :: Lookups.ScriptLookups
      lookups = mempty
    unbalancedTx <- mkUnbalancedTx lookups constraints
    balancedTx <- balanceTxWithConstraints unbalancedTx
      -- just to check that attaching datums works
      ( mustSendChangeWithDatum $ OutputDatum $ Datum $ Integer $ BigInt.fromInt
          1000
      )
    balancedSignedTx <- signTransaction balancedTx
    let outputs = balancedTx ^. to unwrap <<< _body <<< _outputs
    length outputs `shouldEqual` expectedOutputs
    txHash <- submit balancedSignedTx
    awaitTxConfirmed txHash
