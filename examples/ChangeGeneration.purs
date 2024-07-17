module Ctl.Examples.ChangeGeneration (checkChangeOutputsDistribution) where

import Prelude

import Cardano.Transaction.Builder (TransactionBuilderStep(Pay))
import Cardano.Types
  ( Credential(ScriptHashCredential)
  , OutputDatum(OutputDatumHash)
  , PaymentCredential(PaymentCredential)
  , TransactionOutput(TransactionOutput)
  , _body
  , _outputs
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.PlutusData as PlutusData
import Contract.Address (mkAddress)
import Contract.BalanceTxConstraints (mustSendChangeWithDatum)
import Contract.Monad (Contract, liftedM)
import Contract.PlutusData (OutputDatum(OutputDatum), PlutusData(Integer))
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTx
  , buildTx
  , signTransaction
  , submit
  )
import Contract.Value as Value
import Contract.Wallet (getWalletAddress)
import Ctl.Examples.AlwaysSucceeds as AlwaysSucceeds
import Data.Array (length, replicate)
import Data.Lens ((^.))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import JS.BigInt (fromInt) as BigInt
import Test.Spec.Assertions (shouldEqual)

-- | A contract that creates `outputsToScript` number of outputs at a script address,
-- | `outputsToSelf` outputs going to own address, and asserts that the number of change
-- | outputs is equal to `expectedOutputs`.
checkChangeOutputsDistribution :: Int -> Int -> Int -> Contract Unit
checkChangeOutputsDistribution outputsToScript outputsToSelf expectedOutputs =
  do
    validator <- AlwaysSucceeds.alwaysSucceedsScript
    address <- liftedM "Failed to get own address" $ getWalletAddress
    let
      vhash = validatorHash validator
    scriptAddress <- mkAddress
      (PaymentCredential $ ScriptHashCredential $ vhash)
      Nothing
    let
      value = Value.lovelaceValueOf $ BigNum.fromInt 1000001

      plan =
        replicate outputsToSelf
          ( Pay $ TransactionOutput
              { address: address
              , amount: value
              , datum: Nothing
              , scriptRef: Nothing
              }
          ) <>
          replicate outputsToScript
            ( Pay $ TransactionOutput
                { address: scriptAddress
                , amount: value
                , datum: Just $ OutputDatumHash $ hashPlutusData PlutusData.unit
                , scriptRef: Nothing
                }
            )

    unbalancedTx <- buildTx plan
    balancedTx <- balanceTx unbalancedTx Map.empty
      -- just to check that attaching datums works
      ( mustSendChangeWithDatum $ OutputDatum $ Integer $ BigInt.fromInt
          1000
      )
    balancedSignedTx <- signTransaction balancedTx
    let outputs = balancedTx ^. _body <<< _outputs
    length outputs `shouldEqual` expectedOutputs
    txHash <- submit balancedSignedTx
    awaitTxConfirmed txHash
