module Ctl.Examples.Helpers
  ( mkAssetName
  , mustPayToPubKeyStakeAddress
  , mustPayToPubKeyStakeAddressWithDatum
  , mustPayToPubKeyStakeAddressWithScriptRef
  , submitAndLog
  ) where

import Contract.Prelude

import Cardano.Types
  ( AssetName
  , PaymentPubKeyHash
  , PlutusData
  , ScriptRef
  , StakePubKeyHash
  , Transaction
  , Value
  )
import Cardano.Types.AssetName as AssetName
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Transaction (awaitTxConfirmed, submit)
import Contract.TxConstraints (DatumPresence)
import Contract.TxConstraints as Constraints

mkAssetName :: String -> Contract AssetName
mkAssetName str =
  liftContractM ("Cannot make token name from: " <> str)
    $ AssetName.mkAssetName
    =<< byteArrayFromAscii str

mustPayToPubKeyStakeAddress
  :: PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> Value
  -> Constraints.TxConstraints
mustPayToPubKeyStakeAddress pkh Nothing =
  Constraints.mustPayToPubKey pkh
mustPayToPubKeyStakeAddress pkh (Just skh) =
  Constraints.mustPayToPubKeyAddress pkh skh

mustPayToPubKeyStakeAddressWithDatum
  :: PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> PlutusData
  -> DatumPresence
  -> Value
  -> Constraints.TxConstraints
mustPayToPubKeyStakeAddressWithDatum pkh Nothing datum dtp =
  Constraints.mustPayToPubKeyWithDatum pkh datum dtp
mustPayToPubKeyStakeAddressWithDatum pkh (Just skh) datum dtp =
  Constraints.mustPayToPubKeyAddressWithDatum pkh skh datum dtp

mustPayToPubKeyStakeAddressWithScriptRef
  :: PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> ScriptRef
  -> Value
  -> Constraints.TxConstraints
mustPayToPubKeyStakeAddressWithScriptRef pkh Nothing scriptRef =
  Constraints.mustPayToPubKeyWithScriptRef pkh scriptRef
mustPayToPubKeyStakeAddressWithScriptRef pkh (Just skh) scriptRef =
  Constraints.mustPayToPubKeyAddressWithScriptRef pkh skh scriptRef

submitAndLog :: Transaction -> Contract Unit
submitAndLog bsTx = do
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ "Confirmed Tx ID: " <> show txId
