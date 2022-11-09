module Ctl.Examples.Helpers
  ( buildBalanceSignAndSubmitTx
  , buildBalanceSignAndSubmitTx'
  , mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  , liftedHead
  , maybeArrayToHead
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, StakePubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (class IsData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups, mkUnbalancedTx) as Lookups
import Contract.Scripts (class ValidatorTypes, MintingPolicy)
import Contract.Transaction
  ( TransactionHash
  , balanceTx
  , getTxFinalFee
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (mkTokenName, scriptCurrencySymbol) as Value
import Data.Array (head)
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
  balancedTx <- liftedE $ balanceTx unbalancedTx
  balancedSignedTx <- signTransaction balancedTx
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

maybeArrayToHead :: forall (a :: Type). Maybe (Array a) -> Maybe a
maybeArrayToHead = join <<< liftA1 head

liftedHead
  :: forall (a :: Type) (r :: Row Type)
   . String
  -> Contract r (Maybe (Array a))
  -> Contract r a
liftedHead errorMsg contr = liftedM errorMsg (maybeArrayToHead <$> contr)

