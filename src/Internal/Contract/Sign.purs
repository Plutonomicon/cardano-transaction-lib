module Ctl.Internal.Contract.Sign
  ( signTransaction
  ) where

import Prelude

import Cardano.Types (Transaction, _body, _inputs, _witnessSet)
import Control.Monad.Reader (asks)
import Ctl.Internal.BalanceTx.Sync (isCip30Wallet, syncWalletWithTxInputs)
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Contract.Wallet (withWallet)
import Ctl.Internal.Wallet (Wallet(GenericCip30, KeyWallet))
import Data.Array (fromFoldable)
import Data.Lens ((<>~))
import Data.Lens.Getter ((^.))
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (try)

-- | Signs a transaction with potential failure.
signTransaction
  :: Transaction -> Contract Transaction
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
    GenericCip30 cip30 -> liftAff $ cip30.signTx tx
    KeyWallet kw -> liftAff do
      witnessSet <- (unwrap kw).signTx tx
      pure $ tx # _witnessSet <>~ witnessSet
