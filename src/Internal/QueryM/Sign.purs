module Ctl.Internal.QueryM.Sign
  ( signTransaction
  ) where

import Prelude

import Control.Monad.Reader (asks)
import Ctl.Internal.Cardano.Types.Transaction (_body, _inputs, _witnessSet)
import Ctl.Internal.Cardano.Types.Transaction as Transaction
import Ctl.Internal.Helpers (liftedM)
import Ctl.Internal.QueryM
  ( QueryM
  , callCip30Wallet
  , getWalletAddresses
  , withMWallet
  )
import Ctl.Internal.QueryM.Utxos (getUtxo, getWalletUtxos)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Wallet
  ( Wallet(KeyWallet, Lode, Eternl, Flint, Gero, Nami, NuFi)
  )
import Data.Array (elem, fromFoldable)
import Data.Lens ((<>~))
import Data.Lens.Getter ((^.))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for_, traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (delay, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw, try)

signTransaction
  :: Transaction.Transaction -> QueryM (Maybe Transaction.Transaction)
signTransaction tx = do
  config <- asks (_.config)
  let
    runHook =
      for_ config.hooks.beforeSign (void <<< liftEffect <<< try)
  runHook
  withMWallet case _ of
    Nami nami -> commonSignAction nami
    Gero gero -> commonSignAction gero
    Flint flint -> commonSignAction flint
    Eternl eternl -> do
      let
        txInputs :: Array TransactionInput
        txInputs = fromFoldable $ tx ^. _body <<< _inputs
      walletWaitForInputs txInputs
      commonSignAction eternl
    Lode lode -> commonSignAction lode
    NuFi nufi -> commonSignAction nufi
    KeyWallet kw -> liftAff do
      witnessSet <- (unwrap kw).signTx tx
      pure $ Just (tx # _witnessSet <>~ witnessSet)
  where
  commonSignAction cip30 =
    liftAff $ callCip30Wallet cip30 \nw -> (\x -> nw.signTx x tx true)

-- | Waits till all provided inputs of a given transaction appear in the UTxO
-- | set provided by the wallet.
-- | This is a hacky solution to the problem of Eternl not seeing UTxOs that
-- | hasn't been fully confirmed at the moment of a `sign()` call.
-- | Since it can't detect UTxO origin, it can't decide which of the private
-- | keys to use for signing. As a result, we get `MissingVKeyWitnesses`.
walletWaitForInputs :: Array TransactionInput -> QueryM Unit
walletWaitForInputs txInputs = do
  ownAddrs <- getWalletAddresses Nothing
  ownInputUtxos <- txInputs #
    traverse
      ( \txInput -> do
          utxo <- liftedM (error "Could not get utxo") $ getUtxo txInput
          pure (txInput /\ utxo)
      ) >>> map
      ( Map.fromFoldable >>> Map.filter
          ( flip elem ownAddrs
              <<< _.address
              <<< unwrap
          )
      )
  let
    go attempts = do
      walletUtxos <- getWalletUtxos Nothing Nothing <#> fromMaybe Map.empty
      unless (ownInputUtxos `Map.isSubmap` walletUtxos) do
        when (attempts == 0) do
          liftEffect $ throw $
            "walletWaitForInputs: timeout while waiting for wallet"
              <> " UTxO set and CTL query layer UTxO set to synchronize. UTxOs"
              <> " from Ogmios: "
              <> show ownInputUtxos
              <> ", UTxOs from wallet: "
              <> show walletUtxos
              <> ", UTxOs that didn't appear in the wallet: "
              <>
                show (Map.difference ownInputUtxos walletUtxos)
        liftAff $ delay $ wrap $ 1000.0
        go (attempts - 1)
  -- As clarified in Eternl discord, they synchronize with the server every 2
  -- minutes, so 150 seconds would probably be enough to also account for
  -- possible network latency.
  go 150
