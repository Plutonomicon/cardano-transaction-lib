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

import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.Prelude (for)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.Contract.Wallet (getWalletBalance, getWalletUtxos) as Utxos
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusAddress
  , toPlutusTxOutput
  , toPlutusUtxoMap
  )
import Ctl.Internal.Plutus.Conversion.Value (toPlutusValue)
import Ctl.Internal.Plutus.Types.Address (class PlutusAddress, getAddress)
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutput, UtxoMap)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap) as X
import Ctl.Internal.Plutus.Types.Value (Value)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Maybe (Maybe)
import Effect.Aff.Class (liftAff)

-- | Queries for utxos at the given Plutus `Address`.
utxosAt
  :: forall (address :: Type)
   . PlutusAddress address
  => address
  -> Contract UtxoMap
utxosAt address = do
  networkId <- asks _.networkId
  queryHandle <- getQueryHandle
  let cardanoAddr = fromPlutusAddress networkId (getAddress address)
  cardanoUtxoMap <- liftedE $ liftAff $ queryHandle.utxosAt cardanoAddr
  liftContractM "utxosAt: failed to convert utxos"
    $ toPlutusUtxoMap cardanoUtxoMap

-- | Queries for an utxo given a transaction input.
-- | Returns `Nothing` if the output has already been spent.
getUtxo
  :: TransactionInput
  -> Contract (Maybe TransactionOutput)
getUtxo oref = do
  queryHandle <- getQueryHandle
  cardanoTxOutput <- liftedE $ liftAff $ queryHandle.getUtxoByOref oref
  for cardanoTxOutput
    (liftContractM "getUtxo: failed to convert tx output" <<< toPlutusTxOutput)

getWalletBalance
  :: Contract (Maybe Value)
getWalletBalance = Utxos.getWalletBalance <#> map toPlutusValue

-- | Similar to `utxosAt` called on own address, except that it uses CIP-30
-- | wallet state and not query layer state.
-- | The user should not expect these states to be in sync.
-- | When active wallet is `KeyWallet`, query layer state is used.
-- | This function is expected to be more performant than `utxosAt` when there
-- | is a large number of assets.
getWalletUtxos
  :: Contract (Maybe UtxoMap)
getWalletUtxos = do
  mCardanoUtxos <- Utxos.getWalletUtxos
  for mCardanoUtxos $
    liftContractM "getWalletUtxos: unable to deserialize UTxOs" <<<
      toPlutusUtxoMap
