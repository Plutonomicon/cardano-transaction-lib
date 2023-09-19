module Ctl.Examples.ChangeGeneration (checkChangeOutputsDistribution) where

import Prelude

import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( _body
  , _outputs
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value as Value
import Contract.Wallet (ownPaymentPubKeyHashes, ownStakePubKeyHashes)
import Ctl.Examples.AlwaysSucceeds as AlwaysSucceeds
import Data.Array (fold, replicate, zip)
import Data.Array (length) as Array
import Data.BigInt (fromInt) as BigInt
import Data.Lens (to, (^.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
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

      constraintsToSelf :: TxConstraints Unit Unit
      constraintsToSelf = fold <<< fold $ replicate outputsToSelf
        $ zip pkhs skhs <#> \(Tuple pkh mbSkh) -> case mbSkh of
            Nothing -> Constraints.mustPayToPubKey pkh value
            Just skh -> Constraints.mustPayToPubKeyAddress pkh skh value

      constraintsToScripts :: TxConstraints Unit Unit
      constraintsToScripts = fold $ replicate outputsToScript
        $ Constraints.mustPayToScript vhash unitDatum
            Constraints.DatumWitness
            value

      constraints = constraintsToSelf <> constraintsToScripts

      lookups :: Lookups.ScriptLookups PlutusData
      lookups = mempty
    unbalancedTx <- liftedE $ mkUnbalancedTx lookups constraints
    balancedTx <- liftedE $ balanceTx unbalancedTx
    let outputs = balancedTx ^. to unwrap <<< _body <<< _outputs
    Array.length outputs `shouldEqual` expectedOutputs
    balancedSignedTx <- signTransaction balancedTx
    txHash <- submit balancedSignedTx
    awaitTxConfirmed txHash
