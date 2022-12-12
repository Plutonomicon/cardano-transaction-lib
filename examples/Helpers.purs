module Ctl.Examples.Helpers
  ( mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  , submitAndLog
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, StakePubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups, mkUnbalancedTx) as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Transaction
  ( BalancedSignedTransaction
  , TransactionHash
  , awaitTxConfirmed
  , balanceTx
  , getTxByHash
  , getTxFinalFee
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (mkTokenName, scriptCurrencySymbol) as Value
import Data.BigInt (BigInt)
import Effect.Exception (throw)

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

submitAndLog
  :: forall (r :: Row Type). BalancedSignedTransaction -> Contract r Unit
submitAndLog bsTx = do
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  awaitTxConfirmed txId
  mbTransaction <- getTxByHash txId
  logInfo' $ "Retrieved tx: " <> show mbTransaction
  liftEffect $ when (isNothing mbTransaction) do
    void $ throw "Unable to get Tx contents"
    when (mbTransaction /= Just (unwrap bsTx)) do
      throw "Tx contents do not match"

