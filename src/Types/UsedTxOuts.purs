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
import Control.Applicative (unless)
import Control.Bind (bind, (=<<), (>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.RWS (ask)
import Control.Monad.Reader (class MonadAsk)
import Data.Array (concatMap)
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
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.Unit (Unit)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prelude (map, not, discard, otherwise, (<>))
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

-- | `lockRemainingTransactionInputs alreadyLocked tx`
-- |
-- | Mark 'tx's inputs as used, except for those which
-- | are contained in 'alreadylocked' (which have been
-- | locked in a previous step).
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
    outRefs :: Set { transactionId :: TransactionHash, index :: UInt }
    outRefs =
      Set.filter (not $ cacheContains $ unwrap alreadyLocked) $ txOutRefs tx

    updateCache :: TxOutRefCache -> { state :: TxOutRefCache, value :: Boolean }
    updateCache cache
      | all (isUnlocked cache) outRefs =
          { state: foldr insertCache cache $ outRefs, value: true }
      | otherwise = { state: cache, value: false }

    isUnlocked cache { transactionId, index } = maybe true
      (not $ Set.member index)
      (Map.lookup transactionId cache)

    refsToTxOut
      :: Set { transactionId :: TransactionHash, index :: UInt }
      -> TxOutRefCache
    refsToTxOut = foldr insertCache Map.empty

    insertCache
      :: { transactionId :: TransactionHash, index :: UInt }
      -> TxOutRefCache
      -> TxOutRefCache
    insertCache { transactionId, index } = Map.alter
      (fromMaybe Set.empty >>> Set.insert index >>> Just)
      transactionId

  in
    do
      cache <- unwrap <$> ask
      success <- liftEffect $ Ref.modify' updateCache cache
      unless success $ liftEffect $ throw
        "Transaction inputs locked by another transaction"
      pure $ (wrap $ refsToTxOut outRefs) <> alreadyLocked

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

-- | Lock the inputs of a transaction and then run a monadic action.
-- | Will throw `Error` if any of the inputs are already locked.
-- | In case of any error, locks will be released.
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
  catchError f \e -> do
    unlockTxOutKeys used
    throwError e

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

-- | Remove used marks from all inputs that are saved in a `TxOutRefUnlockKeys`.
-- | Use this on the result of a previous `lockTransactionInputs`
unlockTxOutKeys
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadAsk UsedTxOuts m
  => MonadEffect m
  => TxOutRefUnlockKeys
  -> m Unit
unlockTxOutKeys = unlockTxOutRefs <<< cacheToRefs <<< unwrap
  where
  cacheToRefs
    :: TxOutRefCache
    -> Array { transactionId :: TransactionHash, index :: UInt }
  cacheToRefs = concatMap flatten <<< Map.toUnfoldable

  flatten
    :: TransactionHash /\ Set UInt
    -> Array { transactionId :: TransactionHash, index :: UInt }
  flatten (tid /\ indexes) = map ({ transactionId: tid, index: _ })
    (Set.toUnfoldable indexes)

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
  :: Transaction -> Set { transactionId :: TransactionHash, index :: UInt }
txOutRefs tx = Set.map unwrap (unwrap (unwrap tx).body).inputs
