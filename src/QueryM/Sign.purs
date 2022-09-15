module QueryM.Sign
  ( signTransaction
  ) where

import Prelude

import Cardano.Types.Transaction (UtxoMap, _witnessSet)
import Cardano.Types.Transaction as Transaction
import Data.Array (catMaybes) as Array
import Data.Foldable (fold, foldr)
import Data.Lens ((<>~))
import Data.Map as Map
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import QueryM (QueryM, callCip30Wallet, getWalletAddresses, withMWallet)
import QueryM.Utxos (getWalletUtxos, utxosAt)
import Wallet (Wallet(KeyWallet, Lode, Eternl, Flint, Gero, Nami))

signTransaction
  :: Transaction.Transaction -> QueryM (Maybe Transaction.Transaction)
signTransaction tx = withMWallet case _ of
  Nami nami -> liftAff $ callCip30Wallet nami \nw -> flip nw.signTx tx
  Gero gero -> liftAff $ callCip30Wallet gero \nw -> flip nw.signTx tx
  Flint flint -> liftAff $ callCip30Wallet flint \nw -> flip nw.signTx tx
  Eternl eternl -> do
    ogmiosUtxos <- getWalletAddresses >>= fold >>> traverse utxosAt
      >>> map (Array.catMaybes >>> foldr Map.union Map.empty)
    delayUntilUtxosSynchronized ogmiosUtxos
    liftAff $ callCip30Wallet eternl \nw -> flip nw.signTx tx
  Lode lode -> liftAff $ callCip30Wallet lode \nw -> flip nw.signTx tx
  KeyWallet kw -> liftAff do
    witnessSet <- (unwrap kw).signTx tx
    pure $ Just (tx # _witnessSet <>~ witnessSet)

-- | Waits till all UTxOs in a given map appear in the UTxOs provided by wallet.
-- | This is a hacky solution to the problem of Eternl not seeing UTxOs that
-- | hasn't been fully confirmed at the moment of a `sign()` call.
-- | Since it can't detect their origin, it can't know which of the private keys
-- | to use for signing. As a result, we get `MissingVKeyWitnesses`.
delayUntilUtxosSynchronized :: UtxoMap -> QueryM Unit
delayUntilUtxosSynchronized ogmiosUtxos =
  -- As clarified in Eternl discord, they synchronize with the server every 2
  -- minutes, so 150 seconds would probably be enough to also account for
  -- possible network latency.
  go 150
  where
  go attempts = do
    walletUtxos <- getWalletUtxos <#> fromMaybe Map.empty
    unless (ogmiosUtxos `Map.isSubmap` walletUtxos) do
      liftAff $ delay $ wrap $ 1000.0
      when (attempts == 0) do
        liftEffect $ throw $
          "delayUntilUtxosSynchronized: timeout while waiting for wallet"
            <> " UTxO set and CTL query layer UTxO set to synchronize. UTxOs"
            <> " from Ogmios: "
            <> show ogmiosUtxos
            <> ", UTxOs from wallet: "
            <> show walletUtxos
            <> ", UTxOs that didn't appear in the wallet: "
            <>
              show (Map.difference ogmiosUtxos walletUtxos)
      go (attempts - 1)
