module CTL.Examples.Helpers
  ( buildBalanceSignAndSubmitTx
  , buildBalanceSignAndSubmitTx'
  , mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  , mustPayToPubKeyStakeAddressWithDatum
  ) where

import CTL.Contract.Prelude

import CTL.Contract.Address (PaymentPubKeyHash, StakePubKeyHash)
import CTL.Contract.Log (logInfo')
import CTL.Contract.Monad (Contract, liftContractM, liftedE)
import CTL.Contract.PlutusData (Datum)
import CTL.Contract.Prim.ByteArray (byteArrayFromAscii)
import CTL.Contract.ScriptLookups (ScriptLookups, mkUnbalancedTx) as Lookups
import CTL.Contract.Scripts (MintingPolicy)
import CTL.Contract.Transaction
  ( TransactionHash
  , balanceAndSignTxE
  , getTxFinalFee
  , submit
  )
import CTL.Contract.TxConstraints (DatumPresence)
import CTL.Contract.TxConstraints as Constraints
import CTL.Contract.Value (CurrencySymbol, TokenName, Value)
import CTL.Contract.Value (mkTokenName, scriptCurrencySymbol) as Value
import CTL.Internal.IsData (class IsData)
import CTL.Internal.Types.TypedValidator (class ValidatorTypes)
import Data.BigInt (BigInt)

buildBalanceSignAndSubmitTx'
  :: forall (r :: Row Type) (validator :: Type) (datum :: Type)
       (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => Lookups.ScriptLookups validator
  -> Constraints.TxConstraints redeemer datum
  -> Contract r { txHash :: TransactionHash, txFinalFee :: BigInt }
buildBalanceSignAndSubmitTx' lookups constraints = do
  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  balancedSignedTx <- liftedE $ balanceAndSignTxE unbalancedTx
  txHash <- submit balancedSignedTx
  logInfo' $ "Tx ID: " <> show txHash
  pure { txHash, txFinalFee: getTxFinalFee balancedSignedTx }

buildBalanceSignAndSubmitTx
  :: forall (r :: Row Type) (validator :: Type) (datum :: Type)
       (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => Lookups.ScriptLookups validator
  -> Constraints.TxConstraints redeemer datum
  -> Contract r TransactionHash
buildBalanceSignAndSubmitTx lookups constraints =
  _.txHash <$> buildBalanceSignAndSubmitTx' lookups constraints

mkCurrencySymbol
  :: forall (r :: Row Type)
   . Contract r MintingPolicy
  -> Contract r (MintingPolicy /\ CurrencySymbol)
mkCurrencySymbol mintingPolicy = do
  mp <- mintingPolicy
  cs <- liftContractM "Cannot get cs" $ Value.scriptCurrencySymbol mp
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
mustPayToPubKeyStakeAddress pkh Nothing =
  Constraints.mustPayToPubKey pkh
mustPayToPubKeyStakeAddress pkh (Just skh) =
  Constraints.mustPayToPubKeyAddress pkh skh

mustPayToPubKeyStakeAddressWithDatum
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> Datum
  -> DatumPresence
  -> Value
  -> Constraints.TxConstraints i o
mustPayToPubKeyStakeAddressWithDatum pkh Nothing dat dp =
  Constraints.mustPayToPubKeyWithDatum pkh dat dp
mustPayToPubKeyStakeAddressWithDatum pkh (Just skh) dat dp =
  Constraints.mustPayToPubKeyAddressWithDatum pkh skh dat dp
