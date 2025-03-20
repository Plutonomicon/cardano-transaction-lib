-- | This module demonstrates how to enable transaction chaining when submitting
-- | multiple transactions in the correct order; and how subsequent transactions
-- | can use utxos created in the previous ones without waiting for them to
-- | leave the mempool and be included in the block.
module Ctl.Examples.TxChaining
  ( main
  , example
  , contract
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder (TransactionBuilderStep(Pay))
import Cardano.Types
  ( Credential(PubKeyHashCredential)
  , OutputDatum(OutputDatumHash)
  , PaymentCredential(PaymentCredential)
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.PlutusData as PlutusData
import Contract.Address (mkAddress)
import Contract.BalanceTxConstraints
  ( BalancerConstraints
  , mustUseAdditionalUtxos
  )
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTx
  , buildTx
  , createAdditionalUtxos
  , signTransaction
  , submit
  , withBalancedTx
  )
import Contract.Value as Value
import Contract.Wallet (ownPaymentPubKeyHashes)
import Data.Array (head)
import Data.Map as Map
import Effect.Exception (throw)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

contract :: Contract Unit
contract = do
  pkh <- liftedM "Failed to get PKH" $ head <$> ownPaymentPubKeyHashes
  address <- mkAddress (PaymentCredential $ PubKeyHashCredential $ unwrap pkh)
    Nothing
  let
    plan =
      [ Pay $ TransactionOutput
          { address: address
          , amount: Value.lovelaceValueOf $ BigNum.fromInt 1_000_000
          , datum: Just $ OutputDatumHash $ hashPlutusData PlutusData.unit
          , scriptRef: Nothing
          }
      ]

  unbalancedTx0 <- buildTx plan

  withBalancedTx unbalancedTx0 Map.empty mempty \balancedTx0 -> do
    logInfo' $ "balanced"
    balancedSignedTx0 <- signTransaction balancedTx0

    additionalUtxos <- createAdditionalUtxos balancedSignedTx0
    logInfo' $ "Additional utxos: " <> show additionalUtxos
    when (Map.isEmpty additionalUtxos) do
      liftEffect $ throw "empty utxos"
    let
      balanceTxConstraints :: BalancerConstraints
      balanceTxConstraints =
        mustUseAdditionalUtxos additionalUtxos
    unbalancedTx1 <- buildTx plan
    balancedTx1 <- balanceTx unbalancedTx1 additionalUtxos balanceTxConstraints
    balancedSignedTx1 <- signTransaction balancedTx1

    txId0 <- submit balancedSignedTx0
    txId1 <- submit balancedSignedTx1

    awaitTxConfirmed txId0
    awaitTxConfirmed txId1
