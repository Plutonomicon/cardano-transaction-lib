-- | This module balances and signs two transactions at once and demonstrates
-- | the `withBalancedandSignedTxs` bracket. The point is that two different
-- | Utxos will be used for these transactions.
module Ctl.Examples.SignMultiple (example, contract, main) where

import Contract.Prelude

import Cardano.Transaction.Builder (TransactionBuilderStep(Pay))
import Cardano.Types
  ( Credential(PubKeyHashCredential)
  , OutputDatum(OutputDatumHash)
  , PaymentCredential(PaymentCredential)
  , StakeCredential(StakeCredential)
  , Transaction
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.Transaction as Transaction
import Contract.Address (mkAddress)
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo', logWarn')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftedM
  , runContract
  , throwContractError
  )
import Contract.Numeric.BigNum as BigNum
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , buildTx
  , signTransaction
  , submit
  , submitTxFromBuildPlan
  , withBalancedTxs
  )
import Contract.Value (leq)
import Contract.Value as Value
import Contract.Wallet
  ( getWalletUtxos
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  )
import Control.Monad.Reader (asks)
import Data.Array (head)
import Data.Map (Map, filter)
import Data.Map as Map
import Data.Set (Set)
import Data.UInt (UInt)
import Effect.Ref as Ref

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: false }
  }

getLockedInputs
  :: Contract (Map TransactionHash (Set UInt))
getLockedInputs = do
  cache <- asks _.usedTxOuts
  liftEffect $ Ref.read $ unwrap cache

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.SignMultiple"
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  skh <- liftedM "Failed to get own SKH" $ head <$> ownStakePubKeyHashes

  -- Early fail if not enough utxos present for 2 transactions
  unlessM hasSufficientUtxos do
    logWarn' "Insufficient Utxos for 2 transactions"
    createAdditionalUtxos
  address <- mkAddress
    (PaymentCredential $ PubKeyHashCredential $ unwrap pkh)
    (StakeCredential <<< PubKeyHashCredential <<< unwrap <$> skh)
  let
    plan =
      [ Pay $ TransactionOutput
          { address
          , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
          , datum: Just $ OutputDatumHash $ hashPlutusData PlutusData.unit
          , scriptRef: Nothing
          }
      ]

  unbalancedTx0 <- buildTx plan
  unbalancedTx1 <- buildTx plan

  txIds <-
    withBalancedTxs
      [ { transaction: unbalancedTx0
        , usedUtxos: Map.empty
        , balancerConstraints: mempty
        }
      , { transaction: unbalancedTx1
        , usedUtxos: Map.empty
        , balancerConstraints: mempty
        }
      ] $ \balancedTxs -> do
      locked <- getLockedInputs
      logInfo' $ "Locked inputs inside bracket (should be nonempty): "
        <> show locked
      traverse (submitAndLog <=< signTransaction) balancedTxs

  locked <- getLockedInputs
  logInfo' $ "Locked inputs after bracket (should be empty): " <> show locked

  case txIds of
    [ txId0, txId1 ] -> do
      awaitTxConfirmed txId0
      logInfo' $ "Tx 0 submitted successfully!"
      awaitTxConfirmed txId1
      logInfo' $ "Tx 1 submitted successfully!"
    _ -> throwContractError "Unexpected error - no transaction IDs"

  where
  submitAndLog
    :: Transaction
    -> Contract TransactionHash
  submitAndLog bsTx = do
    txId <- submit bsTx
    logInfo' $ "Tx ID: " <> show txId
    pure txId

  hasSufficientUtxos :: Contract Boolean
  hasSufficientUtxos = do
    let
      -- 4 Ada: enough to cover 2 Ada transfer and fees
      isUtxoValid u = (Value.lovelaceValueOf $ BigNum.fromInt 4_000_000) `leq`
        (unwrap u).amount

    walletValidUtxos <- liftedM "Failed to get wallet Utxos"
      $ map (filter isUtxoValid)
      <$> getWalletUtxos

    pure $ length walletValidUtxos >= 2 -- 2 transactions

createAdditionalUtxos :: Contract Unit
createAdditionalUtxos = do
  logInfo' "Creating additional UTxOs for SignMultiple example"
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  skh <- liftedM "Failed to get own SKH" $ head <$> ownStakePubKeyHashes
  address <- mkAddress
    (PaymentCredential $ PubKeyHashCredential $ unwrap pkh)
    (StakeCredential <<< PubKeyHashCredential <<< unwrap <$> skh)
  let
    plan =
      [ Pay $ TransactionOutput
          { address
          , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
          , datum: Just $ OutputDatumHash $ hashPlutusData PlutusData.unit
          , scriptRef: Nothing
          }
      , Pay $ TransactionOutput
          { address
          , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
          , datum: Just $ OutputDatumHash $ hashPlutusData PlutusData.unit
          , scriptRef: Nothing
          }
      ]

  tx <- submitTxFromBuildPlan Map.empty mempty plan

  awaitTxConfirmedWithTimeout (wrap 100.0) $ Transaction.hash tx
  logInfo' $ "Tx submitted successfully!"

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
