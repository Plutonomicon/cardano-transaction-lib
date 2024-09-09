module Ctl.Internal.Testnet.DistributeFunds
  ( DistrFundsError
      ( DistrFunds_MaxUtxosPerTxLowerLimitError
      , DistrFunds_AssignUtxoError
      , DistrFunds_MaxRoundsExceededError
      )
  , DistrFundsParams
  , SourceState
  , Tx(Tx)
  , explainDistrFundsError
  , makeDistributionPlan

  -- exported for testing --------------------------------------------
  , AssignUtxoResult
      ( AssignUtxo_Unassigned
      , AssignUtxo_Deferred
      , AssignUtxo_AssignedToSource
      )
  , assignUtxoToSource
  , initSourceState
  , runDistrFundsRound
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(Done, Loop), tailRecM)
import Control.Safely (foldM)
import Data.Array (fromFoldable, snoc) as Array
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(Left, Right))
import Data.Foldable
  ( class Foldable
  , foldMap
  , foldlDefault
  , foldr
  , foldrDefault
  )
import Data.Generic.Rep (class Generic)
import Data.List (List(Cons, Nil))
import Data.List (filter, fromFoldable) as List
import Data.Newtype (class Newtype, modify, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))

type DistrFundsParams wallet amount =
  { maxRounds :: Int
  , maxUtxosPerTx :: Int
  , getUtxoMinAdaForWallet :: wallet -> amount
  , feePerTx :: amount
  }

--

newtype Tx wallet amount = Tx
  { srcWallet :: wallet
  , numUtxos :: Int
  , utxos :: List { wallet :: wallet, amount :: amount }
  }

derive instance Generic (Tx wallet amount) _
derive instance Newtype (Tx wallet amount) _
derive instance (Eq wallet, Eq amount) => Eq (Tx wallet amount)

instance (Show wallet, Show amount) => Show (Tx wallet amount) where
  show = genericShow

instance Functor (Tx wallet) where
  map f (Tx tx) =
    wrap $ tx
      { utxos =
          map (\utxo -> utxo { amount = f utxo.amount })
            tx.utxos
      }

instance Bifunctor Tx where
  bimap f g (Tx tx) =
    wrap $ tx
      { srcWallet = f tx.srcWallet
      , utxos = map (\utxo -> { wallet: f utxo.wallet, amount: g utxo.amount })
          tx.utxos
      }

instance Foldable (Tx wallet) where
  foldl f a = foldlDefault f a
  foldr f a = foldrDefault f a
  foldMap f = foldMap (f <<< _.amount) <<< _.utxos <<< unwrap

instance Traversable (Tx wallet) where
  sequence = sequenceDefault
  traverse f (Tx tx) = ado
    utxos <- traverse
      (\{ wallet, amount } -> { wallet, amount: _ } <$> f amount)
      tx.utxos
    in wrap $ tx { utxos = utxos }

emptyTx :: forall wallet amount. wallet -> Tx wallet amount
emptyTx srcWallet = wrap
  { srcWallet
  , numUtxos: zero
  , utxos: Nil
  }

isTxNonEmpty :: forall wallet amount. Tx wallet amount -> Boolean
isTxNonEmpty (Tx { numUtxos }) = numUtxos > zero

--

type SourceState wallet amount =
  { srcWallet :: wallet
  , leftover :: amount
  , currentTx :: Tx wallet amount
  }

initSourceState
  :: forall wallet amount
   . wallet
  -> amount
  -> SourceState wallet amount
initSourceState srcWallet initFunds =
  { srcWallet
  , leftover: initFunds
  , currentTx: emptyTx srcWallet
  }

resetSourceTx
  :: forall wallet amount
   . SourceState wallet amount
  -> SourceState wallet amount
resetSourceTx src = src { currentTx = emptyTx src.srcWallet }

--

data DistrFundsError wallet amount
  = DistrFunds_MaxUtxosPerTxLowerLimitError
      { maxUtxosPerTx :: Int
      }
  | DistrFunds_AssignUtxoError
      { utxoToAssign :: wallet /\ amount
      , currentSources :: List (SourceState wallet amount)
      }
  | DistrFunds_MaxRoundsExceededError
      { maxRounds :: Int
      }

derive instance Generic (DistrFundsError wallet amount) _
derive instance (Eq wallet, Eq amount) => Eq (DistrFundsError wallet amount)

instance (Show wallet, Show amount) => Show (DistrFundsError wallet amount) where
  show = genericShow

explainDistrFundsError
  :: forall wallet amount
   . Show wallet
  => Show amount
  => DistrFundsError wallet amount
  -> String
explainDistrFundsError = case _ of
  DistrFunds_MaxUtxosPerTxLowerLimitError { maxUtxosPerTx } ->
    "Each DistributeFunds transaction should have space for at least \
    \one target utxo, current maxUtxosPerTx value: "
      <> show maxUtxosPerTx
      <> "."
  DistrFunds_AssignUtxoError { utxoToAssign, currentSources } ->
    "None of the sources are sufficient to cover target utxo: "
      <> show utxoToAssign
      <> ", current sources: "
      <> show currentSources
      <> "."
  DistrFunds_MaxRoundsExceededError { maxRounds } ->
    "Exceeded the upper limit for the maximum number of fund \
    \distribution rounds, current maxRounds value: "
      <> show maxRounds
      <> "."

type DistrFundsRoundResult wallet amount =
  { sources :: List (SourceState wallet amount)
  , deferredTargets :: List (wallet /\ amount)
  }

makeDistributionPlan
  :: forall wallet amount
   . Ord amount
  => Ring amount
  => DistrFundsParams wallet amount
  -> Array (wallet /\ amount)
  -> Array (wallet /\ amount)
  -> Either (DistrFundsError wallet amount) (Array (Array (Tx wallet amount)))
makeDistributionPlan params initSources initTargets
  | params.maxUtxosPerTx < one =
      Left $ DistrFunds_MaxUtxosPerTxLowerLimitError
        { maxUtxosPerTx: params.maxUtxosPerTx
        }
  | otherwise =
      tailRecM worker
        { sources: List.fromFoldable $ uncurry initSourceState <$> initSources
        , targets: List.fromFoldable initTargets
        , rounds: mempty
        , roundIdx: zero
        }
      where
      worker { sources, targets, rounds, roundIdx }
        | roundIdx == params.maxRounds =
            Left $ DistrFunds_MaxRoundsExceededError
              { maxRounds: params.maxRounds
              }
        | otherwise =
            runDistrFundsRound params sources targets <#> \res ->
              let
                completedTxs = List.filter isTxNonEmpty $ _.currentTx <$>
                  res.sources
                rounds' = Array.snoc rounds $ Array.fromFoldable completedTxs
              in
                case res.deferredTargets of
                  Nil -> Done rounds'
                  _ -> Loop
                    { sources: resetSourceTx <$> res.sources
                    , targets: res.deferredTargets
                    , rounds: rounds'
                    , roundIdx: roundIdx + one
                    }

-- | Executes a single round of funds distribution, assigning the
-- | provided target utxos to the sources.
-- |
-- | Some utxo may be deferred until the next round if they cannot be
-- | immediately covered by the sources due to the algorithm's
-- | parameters, even though the sources are sufficient.
-- |
-- | If some of the target utxos cannot be covered by any source, a
-- | `DistrFunds_AssignUtxoError` will be returned.
runDistrFundsRound
  :: forall wallet amount
   . Ord amount
  => Ring amount
  => DistrFundsParams wallet amount
  -> List (SourceState wallet amount)
  -> List (wallet /\ amount)
  -> Either (DistrFundsError wallet amount)
       (DistrFundsRoundResult wallet amount)
runDistrFundsRound params initSources targets =
  foldM
    ( \distrFundsAcc target ->
        let
          assignUtxoRes /\ sourcesUpdated =
            foldr (tryNextSource params target) (AssignUtxo_Unassigned /\ Nil)
              distrFundsAcc.sources
        in
          case assignUtxoRes of
            AssignUtxo_Unassigned ->
              -- Throw an error if none of the sources have a sufficient
              -- `amount` (e.g. of Lovelace) to cover the target.
              Left $ DistrFunds_AssignUtxoError
                { utxoToAssign: target
                , currentSources: distrFundsAcc.sources
                }
            AssignUtxo_Deferred ->
              Right $ distrFundsAcc
                { deferredTargets = Cons target distrFundsAcc.deferredTargets
                }
            AssignUtxo_AssignedToSource _ ->
              Right $ distrFundsAcc
                { sources = sourcesUpdated
                }
    )
    { sources: initSources
    , deferredTargets: Nil
    }
    targets

-- | Determines whether an attempt should be made to assign the
-- | specified target utxo to the next source based on its current
-- | assignment status. Updates the status if it represents an
-- | improvement over the previous one.
tryNextSource
  :: forall wallet amount
   . Ord amount
  => Ring amount
  => DistrFundsParams wallet amount
  -> wallet /\ amount
  -> SourceState wallet amount
  -> AssignUtxoResult wallet amount /\ List (SourceState wallet amount)
  -> AssignUtxoResult wallet amount /\ List (SourceState wallet amount)
tryNextSource params (targetWallet /\ amount) source (acc /\ sources) =
  case acc of
    AssignUtxo_AssignedToSource _ ->
      -- Utxo has already been assigned, skip other sources.
      acc /\ Cons source sources
    _ ->
      let
        targetNormalized =
          targetWallet /\ max (params.getUtxoMinAdaForWallet targetWallet)
            amount
      in
        case acc, assignUtxoToSource params source targetNormalized of
          AssignUtxo_Deferred, AssignUtxo_Unassigned ->
            -- Utxo marked as deferred that cannot be covered by the
            -- current tx should remain deferred.
            AssignUtxo_Deferred /\ Cons source sources
          _, new@(AssignUtxo_AssignedToSource sourceUpdated) ->
            new /\ Cons sourceUpdated sources
          _, new ->
            new /\ Cons source sources

data AssignUtxoResult wallet amount
  -- Utxo cannot be covered by given source / sources.
  = AssignUtxo_Unassigned
  -- Utxo cannot be included in the current transaction, as doing
  -- so would exceed the specified upper limit on the number of
  -- utxos allowed per transaction (maxUtxosPerTx). This utxo will
  -- be deferred until the next round.
  | AssignUtxo_Deferred
  -- Utxo has been successfully assigned to a source.
  | AssignUtxo_AssignedToSource (SourceState wallet amount)

derive instance Generic (AssignUtxoResult wallet amount) _
derive instance (Eq wallet, Eq amount) => Eq (AssignUtxoResult wallet amount)

instance (Show wallet, Show amount) => Show (AssignUtxoResult wallet amount) where
  show = genericShow

-- | Attempts to assign the specified target utxo to the given source.
-- | ADA value of the target utxo is expected to be normalized, i.e.
-- | utxo min-ada requirement should be taken into account.
assignUtxoToSource
  :: forall wallet amount
   . Ord amount
  => Ring amount
  => DistrFundsParams wallet amount
  -> SourceState wallet amount
  -> wallet /\ amount
  -> AssignUtxoResult wallet amount
assignUtxoToSource params source (targetWallet /\ amountNormalized)
  | (source.leftover - params.feePerTx) < amountNormalized =
      AssignUtxo_Unassigned
  | (unwrap source.currentTx).numUtxos + one > params.maxUtxosPerTx =
      AssignUtxo_Deferred
  | otherwise =
      AssignUtxo_AssignedToSource $ source
        { leftover = source.leftover - amountNormalized
        , currentTx = modify
            ( \tx -> tx
                { numUtxos = tx.numUtxos + one
                , utxos = Cons
                    { wallet: targetWallet, amount: amountNormalized }
                    tx.utxos
                }
            )
            source.currentTx
        }
