-- | The set of wallets utxos selected as inputs for any subsequent transaction
-- | might actually be already submitted by a previous one and pending
-- | to be spent.
-- | This module provides a simple, in-memory cache that helps with keeping
-- | submitted utxos in-check.
module Types.UsedTxOuts
  ( UsedTxOuts(UsedTxOuts)
  , TxOutRefCache
  , isTxOutRefUsed
  , lockTransactionInputs
  , LockTransactionError(LockTransactionError)
  , newUsedTxOuts
  , unlockTxOutRefs
  , unlockTransactionInputs
  , withLockedTransactionInputs    
  ) where

import Cardano.Types.Transaction (Transaction(..))
import Control.Alt ((<$>))
import Control.Alternative (guard, pure)
import Control.Applicative (when)
import Control.Bind (bind, (=<<), (>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Error.Class (class MonadError, catchError, throwError, try)
import Control.Monad.RWS (ask)
import Control.Monad.Reader (class MonadAsk, runReaderT)
import Data.Either (Either(Left))
import Data.Foldable (class Foldable, foldr, foldM, all)
import Data.Function (($))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe, fromMaybe, isJust)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.UInt (UInt)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prelude (class Ord, not, discard, otherwise)
import Types.Transaction (TransactionHash)

type TxOutRefCache = Map TransactionHash (Set UInt)

-- | Stores TxOutRefs in a compact map.
newtype UsedTxOuts = UsedTxOuts (Ref TxOutRefCache)

derive instance Newtype UsedTxOuts _

data LockTransactionError = LockTransactionError

-- | Creates a new empty filter.
newUsedTxOuts
  :: forall (m :: Type -> Type) (t :: Type -> Type)
   . MonadEffect m
  => m UsedTxOuts
newUsedTxOuts = UsedTxOuts <$> liftEffect (Ref.new Map.empty)

-- | Mark transaction's inputs as used.
-- | Returns an action which unlocks them again.
lockTransactionInputs
  :: forall (m :: Type -> Type)
   . MonadAsk UsedTxOuts m
  => MonadError Error m
  => MonadEffect m
  => Transaction
  -> m (Effect Unit)
lockTransactionInputs tx =
  let
    outRefs = txOutRefs tx

    updateCache :: TxOutRefCache -> { state :: TxOutRefCache, value :: Boolean }
    updateCache cache
      | all (isUnlocked cache) outRefs = { state: updateCache' cache, value: true }
      | otherwise = { state: cache, value: false }

    updateCache' cache = foldr
      ( \{ transactionId, index } ->
          Map.alter (fromMaybe Set.empty >>> Set.insert index >>> Just)
            transactionId
      )
      cache
      outRefs

    isUnlocked cache { transactionId, index } = maybe true (not $ Set.member index) (Map.lookup transactionId cache)

    releaseAll :: UsedTxOuts -> Effect Unit
    releaseAll cache = runReaderT (unlockTransactionInputs tx) cache

  in
    do
      cache <- unwrap <$> ask
      success <- liftEffect $ Ref.modify' updateCache cache
      when (not success) $ liftEffect $ throw "Transaction inputs locked"
      pure (releaseAll (wrap cache))

withLockedTransactionInputs
  :: forall (m :: Type -> Type) (a :: Type)
     . MonadAsk UsedTxOuts m
     => MonadError Error m
     => MonadEffect m
     => Transaction
     -> m a
     -> m a
withLockedTransactionInputs t f = do
  release <- lockTransactionInputs t
  res <- catchError f (\e -> do
                          liftEffect release
                          throwError e)
  pure res

insertUnique
  :: forall (e :: Type) (a :: Type) (m :: Type -> Type)
   . (Ord a)
  => (MonadError e m)
  => e
  -> a
  -> Set a
  -> m (Set a)
insertUnique e x s
  | x `Set.member` s = throwError e
  | otherwise = pure $ Set.insert x s

-- | Remove transaction's inputs used marks.
unlockTransactionInputs
  :: forall (m :: Type -> Type)
   . MonadAsk UsedTxOuts m
  => MonadEffect m
  => Transaction
  -> m Unit
unlockTransactionInputs = txOutRefs >>> unlockTxOutRefs

-- | Remove used marks from TxOutRefs given directly.
unlockTxOutRefs
  :: forall (m :: Type -> Type) (t :: Type -> Type) (a :: Type)
   . MonadAsk UsedTxOuts m
  => MonadEffect m
  => Foldable t
  => t { transactionId :: TransactionHash, index :: UInt }
  -> m Unit
unlockTxOutRefs txOutRefs' =
  let
    updateCache :: TxOutRefCache -> TxOutRefCache
    updateCache cache = foldr
      ( \{ transactionId, index } ->
          Map.update
            ( Set.delete index >>> \s ->
                if Set.isEmpty s then Nothing else Just s
            )
            transactionId
      )
      cache
      txOutRefs'
  in
    ask >>= (unwrap >>> Ref.modify_ updateCache >>> liftEffect)

-- | Query if TransactionInput is marked as used.
isTxOutRefUsed
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadAsk UsedTxOuts m
  => MonadEffect m
  => { transactionId :: TransactionHash, index :: UInt }
  -> m Boolean
isTxOutRefUsed { transactionId, index } = do
  cache <- liftEffect <<< Ref.read <<< unwrap =<< ask
  pure $ isJust $ do
    indices <- Map.lookup transactionId cache
    guard $ Set.member index indices

txOutRefs
  :: Transaction -> Array { transactionId :: TransactionHash, index :: UInt }
txOutRefs tx = unwrap <$> (unwrap (unwrap tx).body).inputs
