module Ctl.Internal.Testnet.DistributeFunds where

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
  | DistrFunds_AssignUtxoError
      { utxoToAssign :: wallet /\ amount
      , currentSources :: List (SourceState wallet amount)
      }
  | DistrFunds_MaxRoundsExceededError

derive instance Generic (DistrFundsError wallet amount) _
derive instance (Eq wallet, Eq amount) => Eq (DistrFundsError wallet amount)

instance (Show wallet, Show amount) => Show (DistrFundsError wallet amount) where
  show = genericShow

type DistrFundsRoundResult wallet amount =
  { sources :: List (SourceState wallet amount)
  , deferredTargets :: List (wallet /\ amount)
  }

data AssignUtxoResult wallet amount
  = AssignUtxo_Unassigned
  | AssignUtxo_Deferred
  | AssignUtxo_AssignedToSource (SourceState wallet amount)

derive instance Generic (AssignUtxoResult wallet amount) _
derive instance (Eq wallet, Eq amount) => Eq (AssignUtxoResult wallet amount)

instance (Show wallet, Show amount) => Show (AssignUtxoResult wallet amount) where
  show = genericShow

makeDistributionPlan
  :: forall wallet amount
   . Ord amount
  => Ring amount
  => DistrFundsParams wallet amount
  -> Array (wallet /\ amount)
  -> Array (wallet /\ amount)
  -> Either (DistrFundsError wallet amount) (Array (Array (Tx wallet amount)))
makeDistributionPlan params initSources initTargets
  | params.maxUtxosPerTx < one = Left DistrFunds_MaxUtxosPerTxLowerLimitError
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
            Left DistrFunds_MaxRoundsExceededError
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
      -- utxo already assigned, skip other sources
      acc /\ Cons source sources
    _ ->
      let
        targetNormalized =
          targetWallet /\ max (params.getUtxoMinAdaForWallet targetWallet)
            amount
      in
        case acc, assignUtxoToSource params source targetNormalized of
          AssignUtxo_Deferred, AssignUtxo_Unassigned ->
            -- utxo marked as deferred that cannot fit into the current tx
            -- should remain deferred
            AssignUtxo_Deferred /\ Cons source sources
          _, new@(AssignUtxo_AssignedToSource sourceUpdated) ->
            new /\ Cons sourceUpdated sources
          _, new ->
            new /\ Cons source sources

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
