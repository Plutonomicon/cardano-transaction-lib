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

import Contract.Address (getWalletCollateral)
import Contract.Log (logTrace', logWarn')
import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.Prelude (for)
import Contract.Value as Value
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.BalanceTx.Sync
  ( getControlledAddresses
  , isCip30Wallet
  , syncBackendWithWallet
  , withoutSync
  )
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Contract.Wallet (getWalletUtxos) as Wallet
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusAddress
  , toPlutusTxOutput
  , toPlutusUtxoMap
  )
import Ctl.Internal.Plutus.Types.Address (class PlutusAddress, getAddress)
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutput, UtxoMap)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap) as X
import Ctl.Internal.Plutus.Types.Value (Value)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Foldable (fold, foldr)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (member) as Set
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)

-- | Queries for UTxOs at the given `Address`.
-- |
-- | **Note**: calling `utxosAt` on an address controlled by the light
-- | wallet may result in hard-to-debug problems with wallet interactions.
-- | The developers should not assume that all UTxOs that are available on
-- | wallet addresses are actually spendable. See the docs for UTxO locking
-- | in `doc/query-layers.md`. Using `getWalletUtxos` is a way to avoid the
-- | potential problems. This function will raise a warning in the logs if
-- | wallet address is used.
utxosAt
  :: forall (address :: Type)
   . PlutusAddress address
  => address
  -> Contract UtxoMap
utxosAt addressAny = do
  networkId <- asks _.networkId
  let address = fromPlutusAddress networkId $ getAddress addressAny
  queryHandle <- getQueryHandle
  whenM isCip30Wallet do
    walletAddresses <- getControlledAddresses
    when (address `Set.member` walletAddresses) do
      logWarn' $
        "utxosAt: you are calling `utxosAt` on an address controlled by the"
          <> " wallet. This may result in hard-to-debug problems with wallet "
          <> "interactions. The developers should not assume that all UTxOs "
          <> "that are available on wallet addresses are actually spendable. "
          <> "See the docs for UTxO locking in `doc/query-layers.md`. Using "
          <> "`getWalletUtxos` is a way to avoid the potential problems."
  cardanoUtxoMap <- liftedE $ liftAff $ queryHandle.utxosAt address
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
getWalletBalance = do
  logTrace' "getWalletBalance"
  whenM
    ( asks $ _.synchronizationParams
        >>> _.syncBackendWithWallet
        >>> _.beforeCip30Methods
    )
    syncBackendWithWallet
  let
    getUtxoValue = unwrap >>> _.output >>> unwrap >>> _.amount
    sumValues = foldr (Value.unionWith add) mempty
  -- include both spendable UTxOs and collateral
  utxos <- getWalletUtxos <#> fromMaybe Map.empty
  collateralUtxos <- withoutSync getWalletCollateral <#> fold >>> toUtxoMap
  let allUtxos = Map.union utxos collateralUtxos
  pure $ pure $ sumValues $ map getUtxoValue $ Map.values allUtxos
  where
  toUtxoMap = Map.fromFoldable <<< map
    (unwrap >>> \({ input, output }) -> input /\ output)

-- | Similar to `utxosAt` called on own address, except that it uses CIP-30
-- | wallet state and not query layer state.
-- | The user should not expect these states to be in sync.
-- | When active wallet is `KeyWallet`, query layer state is used.
-- | This function is expected to be more performant than `utxosAt` when there
-- | is a large number of assets.
getWalletUtxos
  :: Contract (Maybe UtxoMap)
getWalletUtxos = do
  logTrace' "getWalletUtxos"
  whenM
    ( asks $
        _.synchronizationParams
          >>> _.syncBackendWithWallet
          >>> _.beforeCip30Methods
    )
    syncBackendWithWallet
  mCardanoUtxos <- Wallet.getWalletUtxos
  for mCardanoUtxos $
    liftContractM "getWalletUtxos: unable to deserialize UTxOs" <<<
      toPlutusUtxoMap
