module Ctl.Examples.Helpers
  ( mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  , mustPayToPubKeyStakeAddressWithDatum
  , mustPayToPubKeyStakeAddressWithScriptRef
  , submitAndLog
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, StakePubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (MintingPolicy)
import Contract.Transaction
  ( BalancedSignedTransaction
  , ScriptRef
  , awaitTxConfirmed
  , submit
  )
import Contract.TxConstraints (DatumPresence)
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (mkTokenName, scriptCurrencySymbol) as Value

mkCurrencySymbol
  :: Contract MintingPolicy
  -> Contract (MintingPolicy /\ CurrencySymbol)
mkCurrencySymbol mintingPolicy = do
  mp <- mintingPolicy
  let cs = Value.scriptCurrencySymbol mp
  pure (mp /\ cs)

mkTokenName :: String -> Contract TokenName
mkTokenName =
  liftContractM "Cannot make token name"
    <<< (Value.mkTokenName <=< byteArrayFromAscii)

mustPayToPubKeyStakeAddress
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> Value
  -> Constraints.TxConstraints
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
  -> Constraints.TxConstraints
mustPayToPubKeyStakeAddressWithDatum pkh Nothing datum dtp =
  Constraints.mustPayToPubKeyWithDatum pkh datum dtp
mustPayToPubKeyStakeAddressWithDatum pkh (Just skh) datum dtp =
  Constraints.mustPayToPubKeyAddressWithDatum pkh skh datum dtp

mustPayToPubKeyStakeAddressWithScriptRef
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> ScriptRef
  -> Value
  -> Constraints.TxConstraints
mustPayToPubKeyStakeAddressWithScriptRef pkh Nothing scriptRef =
  Constraints.mustPayToPubKeyWithScriptRef pkh scriptRef
mustPayToPubKeyStakeAddressWithScriptRef pkh (Just skh) scriptRef =
  Constraints.mustPayToPubKeyAddressWithScriptRef pkh skh scriptRef

submitAndLog
  :: BalancedSignedTransaction -> Contract Unit
submitAndLog bsTx = do
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ "Confirmed Tx ID: " <> show txId
