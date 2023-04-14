module Ctl.Internal.Contract.Sign
  ( signTransaction
  ) where

import Prelude

import Control.Monad.Reader (asks)
import Ctl.Internal.BalanceTx.Sync (isCip30Wallet, syncWalletWithTxInputs)
import Ctl.Internal.Cardano.Types.Transaction (_body, _inputs, _witnessSet)
import Ctl.Internal.Cardano.Types.Transaction as Transaction
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Contract.Wallet (withWallet)
import Ctl.Internal.Wallet
  ( Wallet(KeyWallet, Lode, Eternl, Flint, Gero, Nami, NuFi, Lace)
  , callCip30Wallet
  )
import Data.Array (fromFoldable)
import Data.Lens ((<>~))
import Data.Lens.Getter ((^.))
import Data.Maybe (Maybe(Just))
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (try)

signTransaction
  :: Transaction.Transaction -> Contract (Maybe Transaction.Transaction)
signTransaction tx = do
  hooks <- asks _.hooks
  for_ hooks.beforeSign (void <<< liftEffect <<< try)
  whenM isCip30Wallet do
    whenM
      ( asks $ _.synchronizationParams
          >>> _.syncWalletWithTxInputs
          >>> _.beforeCip30Sign
      )
      do
        syncWalletWithTxInputs $ fromFoldable $ tx ^. _body <<< _inputs
  withWallet case _ of
    Nami nami -> liftAff $ callCip30Wallet nami \nw -> flip nw.signTx tx
    Gero gero -> liftAff $ callCip30Wallet gero \nw -> flip nw.signTx tx
    Flint flint -> liftAff $ callCip30Wallet flint \nw -> flip nw.signTx tx
    Eternl eternl -> do
      liftAff $ callCip30Wallet eternl \nw -> flip nw.signTx tx
    Lode lode -> liftAff $ callCip30Wallet lode \nw -> flip nw.signTx tx
    NuFi nufi -> liftAff $ callCip30Wallet nufi \w -> flip w.signTx tx
    Lace nufi -> liftAff $ callCip30Wallet nufi \w -> flip w.signTx tx
    KeyWallet kw -> liftAff do
      witnessSet <- (unwrap kw).signTx tx
      pure $ Just (tx # _witnessSet <>~ witnessSet)
