module Examples.Helpers
  ( buildBalanceSignAndSubmitTx
  , mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, StakePubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractAffM, liftContractM, liftedE)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups, mkUnbalancedTx) as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Transaction (TransactionHash, balanceAndSignTxE, submit)
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (mkTokenName, scriptCurrencySymbol) as Value
import IsData (class IsData)
import Types.TypedValidator (class ValidatorTypes)

buildBalanceSignAndSubmitTx
  :: forall (r :: Row Type) (validator :: Type) (datum :: Type)
       (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => Lookups.ScriptLookups validator
  -> Constraints.TxConstraints redeemer datum
  -> Contract r TransactionHash
buildBalanceSignAndSubmitTx lookups constraints = do
  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  balancedSignedTx <- liftedE $ balanceAndSignTxE unbalancedTx
  txId <- submit balancedSignedTx
  logInfo' $ "Tx ID: " <> show txId
  pure txId

mkCurrencySymbol
  :: forall (r :: Row Type)
   . Contract r MintingPolicy
  -> Contract r (MintingPolicy /\ CurrencySymbol)
mkCurrencySymbol mintingPolicy = do
  mp <- mintingPolicy
  cs <- liftContractAffM "Cannot get cs" $ Value.scriptCurrencySymbol mp
  pure (mp /\ cs)

mkTokenName :: forall (r :: Row Type). String -> Contract r TokenName
mkTokenName =
  liftContractM "Cannot make token name"
    <<< (Value.mkTokenName <=< byteArrayFromAscii)

mustPayToPubKeyStakeAddress
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> Value
  -> Constraints.TxConstraints i o
mustPayToPubKeyStakeAddress pkh Nothing = Constraints.mustPayToPubKey pkh
mustPayToPubKeyStakeAddress pkh (Just stk) =
  Constraints.mustPayToPubKeyAddress pkh stk
