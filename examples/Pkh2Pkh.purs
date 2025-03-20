-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a transaction. It creates a simple transaction that gets
-- | UTxOs from the user's wallet and sends two Ada back to the same wallet address
module Ctl.Examples.Pkh2Pkh (main, contract, example) where

import Contract.Prelude

import Cardano.Transaction.Builder (TransactionBuilderStep(Pay))
import Cardano.Types
  ( OutputDatum(OutputDatumHash)
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.Transaction as Transaction
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.Transaction (awaitTxConfirmedWithTimeout, submitTxFromBuildPlan)
import Contract.Value as Value
import Contract.Wallet (getWalletAddresses)
import Data.Array (head)
import Data.Map as Map

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: false }
  }

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.Pkh2Pkh"
  address <- liftedM "Failed to get own address" $ head <$> getWalletAddresses
  txId <- Transaction.hash <$> submitTxFromBuildPlan Map.empty mempty
    [ Pay $ TransactionOutput
        { address
        , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
        , datum: Just $ OutputDatumHash $ hashPlutusData PlutusData.unit
        , scriptRef: Nothing
        }
    ]
  awaitTxConfirmedWithTimeout (wrap 100.0) txId
  logInfo' $ "Tx submitted successfully!"

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
