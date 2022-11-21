-- | A module that defines the different Utxo `Data.Map`s from transaction
-- | input to transaction output. Furthermore, a helper to get the utxos at
-- | a given `Address` is defined.
module Contract.Utxos
  ( getUtxo
  , getWalletBalance
  , getWalletUtxos
  , utxosAt
  , module X
  ) where

import Prelude

import Contract.Monad (Contract, liftContractM, liftedE, wrapContract)
import Contract.Prelude (for)
import Contract.Transaction (TransactionInput, TransactionOutput)
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusAddress
  , toPlutusTxOutput
  , toPlutusUtxoMap
  )
import Ctl.Internal.Plutus.Conversion.Value (toPlutusValue)
import Ctl.Internal.Plutus.Types.Address (class PlutusAddress, getAddress)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap) as X
import Ctl.Internal.Plutus.Types.Value (Value)
import Ctl.Internal.QueryM (getNetworkId)
import Ctl.Internal.QueryM.Kupo (getUtxoByOref, utxosAt) as Kupo
import Ctl.Internal.QueryM.QueryHandle (getQueryHandle)
import Ctl.Internal.QueryM.Utxos (getWalletBalance, getWalletUtxos) as Utxos
import Data.Maybe (Maybe)

-- | Queries for utxos at the given Plutus `Address`.
utxosAt
  :: forall (r :: Row Type) (address :: Type)
   . PlutusAddress address
  => address
  -> Contract r UtxoMap
utxosAt address = do
  networkId <- wrapContract getNetworkId
  let cardanoAddr = fromPlutusAddress networkId (getAddress address)
  queryHandle <- wrapContract getQueryHandle
  cardanoUtxoMap <- liftedE $ wrapContract $ queryHandle.utxosAt cardanoAddr
  toPlutusUtxoMap cardanoUtxoMap
    # liftContractM "utxosAt: failed to convert utxos"

-- | Queries for an utxo given a transaction input.
-- | Returns `Nothing` if the output has already been spent.
getUtxo
  :: forall (r :: Row Type)
   . TransactionInput
  -> Contract r (Maybe TransactionOutput)
getUtxo oref = do
  cardanoTxOutput <- liftedE $ wrapContract $ Kupo.getUtxoByOref oref
  for cardanoTxOutput
    (liftContractM "getUtxo: failed to convert tx output" <<< toPlutusTxOutput)

getWalletBalance
  :: forall (r :: Row Type)
   . Contract r (Maybe Value)
getWalletBalance = wrapContract (Utxos.getWalletBalance <#> map toPlutusValue)

-- | Similar to `utxosAt` called on own address, except that it uses CIP-30
-- | wallet state and not query layer state.
-- | The user should not expect these states to be in sync.
-- | When active wallet is `KeyWallet`, query layer state is used.
-- | This function is expected to be more performant than `utxosAt` when there
-- | is a large number of assets.
getWalletUtxos
  :: forall (r :: Row Type)
   . Contract r (Maybe UtxoMap)
getWalletUtxos = do
  mCardanoUtxos <- wrapContract Utxos.getWalletUtxos
  for mCardanoUtxos $
    liftContractM "getWalletUtxos: unable to deserialize UTxOs" <<<
      toPlutusUtxoMap
