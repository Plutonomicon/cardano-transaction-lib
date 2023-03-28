-- | The set of wallets utxos selected as inputs for any subsequent transaction
-- | might actually be already submitted by a previous one and pending
-- | to be spent.
-- | This module provides a simple interface to mark and un-mark Tx inputs as
-- | used.
module Ctl.Internal.Types.UsedTxOuts
  ( lockTransactionInputs
  , unlockTransactionInputs
  ) where

import Prelude

import Aeson (finiteNumber)
import Control.Monad.Error.Class (liftEither)
import Ctl.Internal.Cardano.Types.Transaction (Transaction, _body, _inputs)
import Ctl.Internal.UsedTxOuts as UsedTxOuts
import Ctl.Internal.UsedTxOuts.Storage (Storage)
import Data.DateTime.Instant (unInstant)
import Data.Either (note)
import Data.Foldable (all, foldr)
import Data.Lens ((^.))
import Data.Maybe (isNothing)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Exception (error)
import Effect.Now (now)

-- | Marks transaction's inputs as used.
-- | If some of them are used, returns `false` and does not mark the remaining.
lockTransactionInputs
  :: forall (m :: Type -> Type)
   . Storage
  -> Transaction
  -> Effect Boolean
lockTransactionInputs storage tx = do
  mbTimestamp <- now <#> unInstant >>> unwrap >>> finiteNumber
  timestamp <- liftEither $ note (error tsError) mbTimestamp
  usedTxOuts <- UsedTxOuts.get storage
  let
    inputs = tx ^. _body <<< _inputs
    isUnlocked input = isNothing $ UsedTxOuts.lookup input usedTxOuts
  if all isUnlocked inputs then do
    let usedTxOuts' = foldr (UsedTxOuts.insert timestamp) usedTxOuts inputs
    UsedTxOuts.set storage usedTxOuts'
    pure true
  else
    pure false
  where
  tsError = "Impossible happened: unable to convert timestamp to finite number"

-- | Un-mark transaction's inputs as used, if they are marked.
unlockTransactionInputs
  :: forall (m :: Type -> Type)
   . Storage
  -> Transaction
  -> Effect Unit
unlockTransactionInputs storage tx = do
  let
    inputs = tx ^. _body <<< _inputs
  usedTxOuts <- UsedTxOuts.get storage
  let
    usedTxOuts' = foldr UsedTxOuts.delete usedTxOuts inputs
  UsedTxOuts.set storage usedTxOuts'
