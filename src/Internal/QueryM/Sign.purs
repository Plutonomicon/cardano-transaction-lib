module Ctl.Internal.QueryM.Sign
  ( signTransaction
  ) where

import Prelude

import Control.Monad.Reader (asks)
import Ctl.Internal.Cardano.Types.Transaction (_witnessSet)
import Ctl.Internal.Cardano.Types.Transaction as Transaction
import Ctl.Internal.QueryM
  ( QueryM
  , callCip30Wallet
  , withMWallet
  )
import Ctl.Internal.Wallet
  ( Wallet(KeyWallet, Lode, Eternl, Flint, Gero, Nami, NuFi)
  )
import Data.Lens ((<>~))
import Data.Maybe (Maybe(Just))
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (try)

signTransaction
  :: Transaction.Transaction -> QueryM (Maybe Transaction.Transaction)
signTransaction tx = do
  config <- asks (_.config)
  let
    runHook =
      for_ config.hooks.beforeSign (void <<< liftEffect <<< try)
  runHook
  withMWallet case _ of
    Nami nami -> liftAff $ callCip30Wallet nami \nw -> flip nw.signTx tx
    Gero gero -> liftAff $ callCip30Wallet gero \nw -> flip nw.signTx tx
    Flint flint -> liftAff $ callCip30Wallet flint \nw -> flip nw.signTx tx
    Eternl eternl -> liftAff $ callCip30Wallet eternl \nw -> flip nw.signTx tx
    Lode lode -> liftAff $ callCip30Wallet lode \nw -> flip nw.signTx tx
    NuFi nufi -> liftAff $ callCip30Wallet nufi \w -> flip w.signTx tx
    KeyWallet kw -> liftAff do
      witnessSet <- (unwrap kw).signTx tx
      pure $ Just (tx # _witnessSet <>~ witnessSet)

