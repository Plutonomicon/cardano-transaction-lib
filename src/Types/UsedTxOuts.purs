-- | The set of wallets utxos selected as inputs for any subsequent transaction
-- | might actually be already submitted by a previous one and pending
-- | to be spent.
-- | This module provides a simple, in-memory cache that helps with keeping
-- | submitted utxos in-check.
module Types.UsedTxOuts
  ( UsedTxOuts(UsedTxOuts)
  , TxOutRefUnlockKeys(TxOutRefUnlockKeys)
  , TxOutRefCache
  , isTxOutRefUsed
  , lockTransactionInputs
  , lockRemainingTransactionInputs
  , LockTransactionError(LockTransactionError)
  , newUsedTxOuts
  , unlockTxOutRefs
  , unlockTxOutKeys
  , unlockTransactionInputs
  , withLockedTransactionInputs
  ) where

import Cardano.Types.Transaction (Transaction)
import Control.Alt ((<$>))
import Control.Alternative (guard, pure)
import Control.Applicative (when)
import Control.Bind (bind, (=<<), (>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.RWS (ask)
import Control.Monad.Reader (class MonadAsk)
import Data.Array (concatMap, filter)
import Data.Foldable (class Foldable, foldr, all)
import Data.Function (($))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe, fromMaybe, isJust)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Semigroup (class Semigroup)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(Tuple))
import Data.UInt (UInt)
import Data.Unit (Unit)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prelude (map, not, discard, otherwise)
import Types.Transaction (TransactionHash)

type TxOutRefCache = Map TransactionHash (Set UInt)
newtype TxOutRefUnlockKeys = TxOutRefUnlockKeys TxOutRefCache

derive instance Newtype TxOutRefUnlockKeys _

instance Semigroup TxOutRefUnlockKeys where
  append k1 k2 = wrap $ Map.unionWith Set.union (unwrap k1) (unwrap k2)

instance Monoid TxOutRefUnlockKeys where
  mempty = wrap Map.empty

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

insertCache
  :: { transactionId :: TransactionHash, index :: UInt }
  -> TxOutRefCache
  -> TxOutRefCache
insertCache { transactionId, index } = Map.alter
  (fromMaybe Set.empty >>> Set.insert index >>> Just)
  transactionId

lockRemainingTransactionInputs
  :: forall (m :: Type -> Type)
   . MonadAsk UsedTxOuts m
  => MonadError Error m
  => MonadEffect m
  => TxOutRefUnlockKeys
  -> Transaction
  -> m TxOutRefUnlockKeys
lockRemainingTransactionInputs alreadyLocked tx =
  let
    outRefs = txOutRefs tx

    updateCache :: TxOutRefCache -> { state :: TxOutRefCache, value :: Boolean }
    updateCache cache
      | all (isUnlocked cache) outRefs =
          { state: updateCache' cache, value: true }
      | otherwise = { state: cache, value: false }

    updateCache' cache = foldr insertCache cache
      $ filter (not $ cacheContains $ unwrap alreadyLocked)
      $ outRefs

    isUnlocked cache { transactionId, index } = maybe true
      (not $ Set.member index)
      (Map.lookup transactionId cache)

  in
    do
      cache <- unwrap <$> ask
      success <- liftEffect $ Ref.modify' updateCache cache
      when (not success) $ liftEffect $ throw
        "Transaction inputs locked by another transaction"
      pure (wrap $ refsToTxOut outRefs)

-- | Mark transaction's inputs as used.
-- | Returns the set of TxOuts that was locked by this call
lockTransactionInputs
  :: forall (m :: Type -> Type)
   . MonadAsk UsedTxOuts m
  => MonadError Error m
  => MonadEffect m
  => Transaction
  -> m TxOutRefUnlockKeys
lockTransactionInputs = lockRemainingTransactionInputs mempty

withLockedTransactionInputs
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadAsk UsedTxOuts m
  => MonadError Error m
  => MonadEffect m
  => Transaction
  -> m a
  -> m a
withLockedTransactionInputs t f = do
  used <- lockTransactionInputs t
  res <- catchError f
    ( \e -> do
        unlockTxOutKeys used
        throwError e
    )
  pure res

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

unlockTxOutKeys
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadAsk UsedTxOuts m
  => MonadEffect m
  => TxOutRefUnlockKeys
  -> m Unit
unlockTxOutKeys = unlockTxOutRefs <<< cacheToRefs <<< unwrap

cacheContains
  :: TxOutRefCache
  -> { transactionId :: TransactionHash, index :: UInt }
  -> Boolean
cacheContains cache { transactionId, index } = isJust $ do
  indices <- Map.lookup transactionId cache
  guard $ Set.member index indices

-- | Query if TransactionInput is marked as used.
isTxOutRefUsed
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadAsk UsedTxOuts m
  => MonadEffect m
  => { transactionId :: TransactionHash, index :: UInt }
  -> m Boolean
isTxOutRefUsed ref = do
  cache <- liftEffect <<< Ref.read <<< unwrap =<< ask
  pure (cacheContains cache ref)

txOutRefs
  :: Transaction -> Array { transactionId :: TransactionHash, index :: UInt }
txOutRefs tx = unwrap <$> (unwrap (unwrap tx).body).inputs

refsToTxOut
  :: Array { transactionId :: TransactionHash, index :: UInt } -> TxOutRefCache
refsToTxOut = foldr insertCache Map.empty

cacheToRefs
  :: TxOutRefCache -> Array { transactionId :: TransactionHash, index :: UInt }
cacheToRefs cache = concatMap flatten $ Map.toUnfoldable cache
  where
  flatten (Tuple tid indexes) = map (\ix -> { transactionId: tid, index: ix })
    (Set.toUnfoldable indexes)
