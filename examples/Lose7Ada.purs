-- | Warning: This contract will permanently lock 7 Ada
-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a failing smart-contract transaction. It creates a
-- | transaction that pays two Ada to the `AlwaysFails` script address, and
-- | then attempts to spend the two Ada, failing and losing the collateral.
module Ctl.Examples.Lose7Ada
  ( main
  , example
  , alwaysFailsScript
  , payToAlwaysFails
  , spendFromAlwaysFails
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( OutputWitness(PlutusScriptOutput)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(SpendOutput, Pay)
  )
import Cardano.Types
  ( Credential(ScriptHashCredential)
  , OutputDatum(OutputDatum)
  , PaymentCredential(PaymentCredential)
  , TransactionOutput(TransactionOutput)
  , _isValid
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionUnspentOutput (toUtxoMap)
import Contract.Address (mkAddress)
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , balanceTx
  , buildTx
  , lookupTxHash
  , signTransaction
  , submit
  , submitTxFromBuildPlan
  )
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf, minus) as Value
import Contract.Wallet (getWalletBalance)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Foldable (fold)
import Data.Functor ((<$>))
import Data.Lens ((.~))
import Data.Map as Map
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg do
    logInfo' "Running Examples.AlwaysFails"
    validator <- alwaysFailsScript
    let vhash = validatorHash validator
    logInfo' "Attempt to lock value"
    txId <- payToAlwaysFails vhash
    awaitTxConfirmed txId
    logInfo' "Tx submitted successfully, Try to spend locked values"
    spendFromAlwaysFails vhash validator txId

payToAlwaysFails :: ValidatorHash -> Contract TransactionHash
payToAlwaysFails vhash = do
  scriptAddress <-
    mkAddress (PaymentCredential $ ScriptHashCredential vhash) Nothing
  Transaction.hash <$> submitTxFromBuildPlan Map.empty mempty
    [ Pay $ TransactionOutput
        { address: scriptAddress
        , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
        , datum: Just $ OutputDatum PlutusData.unit
        , scriptRef: Nothing
        }
    ]

spendFromAlwaysFails
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract Unit
spendFromAlwaysFails vhash validator txId = do
  balanceBefore <- unsafePartial $ fold <$> getWalletBalance
  scriptAddress <- mkAddress (wrap $ ScriptHashCredential vhash) Nothing
  utxos <- utxosAt scriptAddress
  utxo <-
    liftM
      ( error
          ( "The id "
              <> show txId
              <> " does not have output locked at: "
              <> show scriptAddress
          )
      )
      $ head (lookupTxHash txId utxos)
  unbalancedTx <-
    buildTx
      [ SpendOutput
          utxo
          $ Just
          $ PlutusScriptOutput (ScriptValue validator) RedeemerDatum.unit
              Nothing
      ] <#> _isValid .~ false
  spendTx <- balanceTx unbalancedTx (toUtxoMap [ utxo ]) mempty
  signedTx <- signTransaction (spendTx # _isValid .~ true)
  spendTxId <- submit signedTx
  logInfo' $ "Tx ID: " <> show spendTxId
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."
  balance <- unsafePartial $ fold <$> getWalletBalance
  let collateralLoss = Value.lovelaceValueOf $ BigNum.fromInt (5_000_000)
  Just balance `shouldEqual` (Value.minus balanceBefore collateralLoss)

alwaysFailsScript :: Contract Validator
alwaysFailsScript = do
  liftMaybe (error "Error decoding alwaysFails") do
    envelope <- decodeTextEnvelope alwaysFails
    plutusScriptFromEnvelope envelope

alwaysFails :: String
alwaysFails =
  """
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "581e581c01000033223232222350040071235002353003001498498480048005"
}
"""
