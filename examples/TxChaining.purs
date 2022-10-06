module Ctl.Examples.TxChaining
  ( main
  , example
  , createAdditionalUtxos
  , contract
  ) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractM
  , liftedE
  , liftedM
  , runContract
  )
import Contract.PlutusData (PlutusData)
import Contract.ScriptLookups as Lookups
import Contract.Test.E2E (publishTestFeedback)
import Contract.Transaction
  ( Transaction
  , TransactionInput(TransactionInput)
  , awaitTxConfirmed
  , balanceAndSignTxE
  , balanceTxWithAdditionalUtxos
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletUtxos)
import Contract.Value as Value
import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.Hashing (transactionHash)
import Ctl.Internal.Plutus.Conversion (toPlutusUtxoMap)
import Ctl.Internal.Serialization (convertTransaction)
import Data.BigInt as BigInt
import Data.Map as Map

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true

contract :: Contract () Unit
contract = do
  pkh <- liftedM "Failed to get PKH" $ ownPaymentPubKeyHash

  wUtxos0 <- liftedM "Failed to get wallet UTXOs" getWalletUtxos
  logInfo' $ "wUtxos0 " <> show wUtxos0

  let
    value :: Value.Value
    value =
      (Value.lovelaceValueOf $ BigInt.fromInt 1_000_000)

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToPubKey pkh value

    lookups0 :: Lookups.ScriptLookups PlutusData
    lookups0 = Lookups.unspentOutputs wUtxos0

  ubTx0 <- liftedE $ Lookups.mkUnbalancedTx lookups0 constraints
  bsTx0 <- liftedE $ balanceAndSignTxE ubTx0

  additionalUtxos <- createAdditionalUtxos (unwrap bsTx0)
  logInfo' $ "Additional UTXOS " <> show additionalUtxos
  additionalUtxos' <-
    liftContractM "Unable to convert to plutus utxo map" $
      toPlutusUtxoMap additionalUtxos

  let
    lookups1 :: Lookups.ScriptLookups PlutusData
    lookups1 = Lookups.unspentOutputs additionalUtxos'

  ubTx1 <- liftedE $ Lookups.mkUnbalancedTx lookups1 constraints
  bTx1 <- liftedE $ balanceTxWithAdditionalUtxos ubTx1 additionalUtxos

  bsTx1 <- liftedM "Unable to sign transaction."
    $ signTransaction
    $ unwrap bTx1

  txId0 <- submit bsTx0
  txId1 <- submit (wrap bsTx1)

  awaitTxConfirmed txId0
  awaitTxConfirmed txId1

createAdditionalUtxos
  :: forall r
   . Transaction
  -> Contract r UtxoMap
createAdditionalUtxos tx = do
  cslTx <- liftEffect $ convertTransaction tx
  pure $
    let
      txHash = transactionHash cslTx
      txBody = tx # unwrap # _.body # unwrap

      txin i =
        TransactionInput
          { transactionId: txHash
          , index: i
          }

      utxoMap = foldl
        (\utxo txout -> Map.insert (txin $ length utxo) txout utxo)
        Map.empty
        txBody.outputs
    in
      utxoMap