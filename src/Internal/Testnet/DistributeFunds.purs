module Ctl.Internal.Testnet.DistributeFunds
  ( makeDistributionPlan
  , parallelizedDistributionPlan
  , SourceState(SourceState)
  , Tx(Tx)
  , _completeTxs
  , _leftover
  , _source
  , _total
  , _totalUtxos
  , _tx
  , _utxos
  -- * Exported for testing purposes
  , assignUtxo
  , emptyTx
  , initialSourceState
  ) where

import Contract.Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Lens (Lens', view, (%~), (+~), (-~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(Cons, Nil))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Effect.Exception.Unsafe (unsafeThrow)
import Type.Proxy (Proxy(Proxy))

newtype Tx src target amount = Tx
  { source :: { key :: src }
  , total :: amount
  , totalUtxos :: Int
  , utxos :: List { key :: target, amount :: amount }
  }

derive instance Newtype (Tx s t a) _
derive instance Generic (Tx s t a) _
derive instance (Eq s, Eq t, Eq a) => Eq (Tx s t a)
derive instance (Ord s, Ord t, Ord a) => Ord (Tx s t a)
instance (Show s, Show t, Show a) => Show (Tx s t a) where
  show = genericShow

emptyTx
  :: forall target amount
   . amount
  -> Tx Unit target amount
emptyTx total = Tx
  { source: { key: unit }
  , total
  , totalUtxos: 0
  , utxos: Nil
  }

newtype SourceState src target amount = SourceState
  { source :: src
  , leftover :: amount
  , tx :: Tx Unit target amount
  , completeTxs :: List (Tx Unit target amount)
  }

derive instance Newtype (SourceState s t a) _
derive instance Generic (SourceState s t a) _
derive instance (Eq s, Eq t, Eq a) => Eq (SourceState s t a)
derive instance (Ord s, Ord t, Ord a) => Ord (SourceState s t a)
instance (Show s, Show t, Show a) => Show (SourceState s t a) where
  show = genericShow

initialSourceState
  :: forall src target amount
   . Semiring amount
  => { initialFunds :: amount, key :: src }
  -> SourceState src target amount
initialSourceState { initialFunds, key } = SourceState
  { source: key
  , leftover: initialFunds
  , tx: emptyTx zero
  , completeTxs: Nil
  }

parallelizedDistributionPlan
  :: forall src target amount
   . Map src (Array (Tx Unit target amount))
  -> Array (Map src (Tx Unit target amount))
parallelizedDistributionPlan _ = unsafeThrow "hello"

makeDistributionPlan
  :: forall src target amount
   . Ord src
  => Ord amount
  => Ord target
  => Ring amount
  => Map src amount
  -> Map target (Array amount)
  -> { maxCoinPerTx :: amount
     , maxTargetUtxosPerTx :: Int
     }
  -> Either
       { err :: String
       , acc :: List (SourceState src target amount)
       }
       (Map src (Array (Tx Unit target amount)))
makeDistributionPlan sources targets thresholds = do
  let
    targetsUtxosAsc :: List { key :: target, amount :: amount }
    targetsUtxosAsc = List.sortBy (flip compare)
      $ Map.toUnfoldable targets
      >>= \(key /\ utxos) ->
        { key, amount: _ } <$> List.fromFoldable utxos

    assigned :: Either _ (Map src (Array (Tx Unit target amount)))
    assigned = do
      sourcesTxs <- foldM
        (flip $ assignUtxo thresholds)
        ( initialSourceState <<< uncurry { key: _, initialFunds: _ } <$>
            Map.toUnfoldable sources
        )
        targetsUtxosAsc
      let
        finish src =
          src
            # (_tx .~ emptyTx zero)
            # (_completeTxs %~ Cons (src ^. _tx))
        sourceToTxs = Map.fromFoldable
          $ Tuple
          <<< view _source
          <*> Array.fromFoldable
          <<< view _completeTxs
          <<< finish
          <$> sourcesTxs
      pure sourceToTxs
  assigned

assignUtxo
  :: forall target src amount
   . Ord amount
  => Ring amount
  => { maxCoinPerTx :: amount
     , maxTargetUtxosPerTx :: Int
     }
  -> { amount :: amount, key :: target }
  -> List (SourceState src target amount)
  -> Either
       { err :: String
       , acc :: List (SourceState src target amount)
       }
       (List (SourceState src target amount))
assignUtxo _ _ Nil = Left
  { err: "Ran out of sources", acc: Nil }
assignUtxo thresholds utxo acc@(Cons source sources)
  | 0 >= thresholds.maxTargetUtxosPerTx =
      Left { err: "maxTargetUtxosPerTx must be greater than 1", acc }
  | utxo.amount >= thresholds.maxCoinPerTx =
      Left
        { err: "UTxO required amount is higher than the maxCoinPerTx threshold"
        , acc
        }
  | (source ^. _tx <<< _totalUtxos)
      >= thresholds.maxTargetUtxosPerTx =
      -- means that this Tx is complete
      assignUtxo thresholds utxo
        $ startNewTx source sources -- be careful: infinite loop
  -- it will terminate because new tx has 0 utxos which is higher than 'maxTargetUtxosPerTx'
  | (source ^. _tx <<< _total) + utxo.amount
      > thresholds.maxCoinPerTx =
      -- means that utxo cannot be fit in this Tx
      let
        -- try fit this utxo in any source
        tryAnother = tryWithAnotherSource
          "Cannot fit UTxO amount into the Tx"
          (assignUtxo thresholds utxo)
          source
          sources
        -- if no source can fit this utxo, create a new tx
        startNew = assignUtxo thresholds utxo
          $ startNewTx source sources -- be careful: infinite loop
      -- it will terminate because either new Tx starting with 0 total can fit it
      -- or the condition above will throw Left
      in
        tryAnother <|> startNew
  | source ^. _leftover < utxo.amount =
      -- means that this source cannot fit this tx
      -- should try with the rest of sources and fail otherwise
      tryWithAnotherSource
        "Not enough funds on sources"
        (assignUtxo thresholds utxo)
        source
        sources
  | otherwise =
      -- means that utxo can be fit into the current tx
      let
        source' = source
          # (_leftover -~ utxo.amount)
          # (_tx <<< _total +~ utxo.amount)
          # (_tx <<< _totalUtxos +~ 1)
          # (_tx <<< _utxos %~ Cons utxo)
      in
        Right $ Cons source' sources

-- * Helpers

-- helper for assignUtxo
tryWithAnotherSource
  :: forall s f
   . Bifunctor f
  => String
  -> (List s -> f { err :: String, acc :: List s } (List s))
  -> s
  -> List s
  -> f { err :: String, acc :: List s } (List s)
tryWithAnotherSource err self source sources =
  bimap (\e -> e { err = err <> "/" <> e.err, acc = Cons source e.acc })
    (Cons source)
    $ self sources

-- helper for assignUtxo
startNewTx
  :: forall src target amount
   . Semiring amount
  => SourceState src target amount
  -> List (SourceState src target amount)
  -> List (SourceState src target amount)
startNewTx source sources =
  List.snoc sources
    $ (_tx .~ emptyTx zero)
    $ (_completeTxs %~ Cons (source ^. _tx))
    $ source

_totalUtxos :: forall s t a. Lens' (Tx s t a) Int
_totalUtxos = _Newtype <<< prop (Proxy :: _ "totalUtxos")

_utxos :: forall s t a. Lens' (Tx s t a) (List { key :: t, amount :: a })
_utxos = _Newtype <<< prop (Proxy :: _ "utxos")

_total :: forall s t a. Lens' (Tx s t a) a
_total = _Newtype <<< prop (Proxy :: _ "total")

_tx :: forall s t a. Lens' (SourceState s t a) (Tx Unit t a)
_tx = _Newtype <<< prop (Proxy :: _ "tx")

_leftover :: forall s t a. Lens' (SourceState s t a) a
_leftover = _Newtype <<< prop (Proxy :: _ "leftover")

_source :: forall s t a. Lens' (SourceState s t a) s
_source = _Newtype <<< prop (Proxy :: _ "source")

_completeTxs :: forall s t a. Lens' (SourceState s t a) (List (Tx Unit t a))
_completeTxs = _Newtype <<< prop (Proxy :: _ "completeTxs")
