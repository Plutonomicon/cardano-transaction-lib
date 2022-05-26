-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address
module Examples.AlwaysSucceeds (main) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Aeson (decodeAeson, fromString)
import Contract.Monad (ContractConfig(ContractConfig), launchAff_, liftContractM, liftedE, liftedM, logInfo', runContract_, traceContractConfig, Contract)
import Contract.PlutusData (PlutusData, unitDatum, unitRedeemer)
import Contract.ScriptLookups (UnattachedUnbalancedTx)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction (BalancedSignedTransaction(BalancedSignedTransaction), balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Contract.Wallet (mkNamiWalletAff)
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Plutus.Types.Transaction (UtxoM(UtxoM))
import Types.Scripts (ValidatorHash)
import Types.Transaction (TransactionInput(..), TransactionHash)
import Types.TxConstraints (TxConstraints)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  cfg <- over ContractConfig _ { wallet = wallet } <$> traceContractConfig
  runContract_ cfg $ do
    logInfo' "Running Examples.AlwaysSucceeds"
    validator <- liftContractM "Invalid script JSON" $ alwaysSucceedsScript
    vhash <- liftContractM "Couldn't hash validator" $ validatorHash validator
    logInfo' "Attempt to lock value "
    txId <- payToAlwaysSucceeds vhash validator
    countToZero 20
    logInfo' "Try to spend locked values"
    spendFromAlwaysSucceeds vhash validator txId

countToZero :: Int -> Contract () Unit 
countToZero n = 
  if n<=0 then 
    pure unit 
  else 
    do
    logInfo' $ "Waiting before we try to unlock : "  <> show n 
    liftAff $ delay $ Milliseconds 1000.0 
    countToZero (n-1)


payToAlwaysSucceeds :: ValidatorHash -> Validator -> Contract () TransactionHash
payToAlwaysSucceeds vhash validator = do
  let
    -- Note that CTL does not have explicit equivalents of Plutus'
    -- `mustPayToTheScript` or `mustPayToOtherScript`, as we have no notion
    -- of a "current" script. Thus, we have the single constraint
    -- `mustPayToScript`, and all scripts must be explicitly provided to build
    -- the transaction (see the value for `lookups` below as well)
    constraints :: Constraints.TxConstraints Unit Unit
    constraints = Constraints.mustPayToScript vhash unitDatum
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator

  balanceSingnAndSubmitTx <<< liftedE
    $ Lookups.mkUnbalancedTx lookups constraints

spendFromAlwaysSucceeds 
  :: ValidatorHash 
  -> Validator -> TransactionHash -> Contract () Unit
spendFromAlwaysSucceeds vhash validator txId = do
  let
    arbitraryRedeemer = unitRedeemer
    scriptAddress = scriptHashAddress vhash
  UtxoM utxos <-
    fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress

  case fst <$> (head <<< Map.toUnfoldable<<< Map.filterWithKey hasTransactionId ) utxos of
    Just oref ->
      let
        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.validator validator
          <> Lookups.unspentOutputs utxos

        constraints :: TxConstraints Unit Unit
        constraints =
          Constraints.mustSpendScriptOutput oref arbitraryRedeemer
      in
        void 
          $ balanceSingnAndSubmitTx 
          <<< liftedE $ Lookups.mkUnbalancedTx lookups constraints
    _ ->
      logInfo' $ "The id : " <> 
        show txId
        <> " does not have output locked at "
        <> show scriptAddress
  where 
    hasTransactionId ::forall t . TransactionInput -> t -> Boolean 
    hasTransactionId (TransactionInput tx) _ = 
      tx.transactionId == txId

balanceSingnAndSubmitTx
  :: Contract () UnattachedUnbalancedTx
  -> Contract () TransactionHash
balanceSingnAndSubmitTx ubTxContract = do
  ubTx <- ubTxContract
  BalancedSignedTransaction bsTx <-
    liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx.signedTxCbor
  logInfo' $ "Tx ID: " <> show txId
  pure txId

alwaysSucceedsScript :: Maybe Validator
alwaysSucceedsScript = map wrap $ hush $ decodeAeson $ fromString
  "4d01000033222220051200120011"
