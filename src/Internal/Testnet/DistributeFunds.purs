module Ctl.Internal.Testnet.DistributeFunds
  ( Tx
  , SourceState
  , parallelizedDistributionPlan
  , makeDistributionPlan
  -- * exported for testing purposes
  , assignUtxo
  , emptyTx
  , initialSourceState
  ) where

import Contract.Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Effect.Exception.Unsafe (unsafeThrow)

type Tx src target amount =
  { source :: { key :: src }
  , total :: amount
  , totalUtxos :: Int
  , utxos :: List { key :: target, amount :: amount }
  }

emptyTx
  :: forall target amount
   . amount
  -> Tx Unit target amount
emptyTx total =
  { source: { key: unit }
  , total
  , totalUtxos: 0
  , utxos: Nil
  }

type SourceState src target amount =
  { source :: src
  , leftover :: amount
  , tx :: Tx Unit target amount
  , completeTxs :: List (Tx Unit target amount)
  }

initialSourceState
  :: forall src target amount
   . Semiring amount
  => { initialFunds :: amount, key :: src }
  -> SourceState src target amount
initialSourceState { initialFunds, key } =
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
            { tx = emptyTx
            , completeTxs = Cons src.tx src.completeTxs
            }
        sourceToTxs = Map.fromFoldable
          $ Tuple
          <<< _.source
          <*> Array.fromFoldable
          <<< _.completeTxs
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
  | source.tx.totalUtxos
      >= thresholds.maxTargetUtxosPerTx =
      -- means that this Tx is complete
      assignUtxo thresholds utxo
        $ startNewTx source sources -- be careful: infinite loop
  -- it will terminate because new tx has 0 utxos which is higher than 'maxTargetUtxosPerTx'
  | source.tx.total + utxo.amount
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
  | source.leftover < utxo.amount =
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
          { leftover = source.leftover - utxo.amount
          , tx = source.tx
              { total = source.tx.total + utxo.amount
              , totalUtxos = source.tx.totalUtxos + 1
              , utxos = Cons utxo source.tx.utxos
              }
          }
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
startNewTx source sources = List.snoc sources source
  { tx = emptyTx zero
  , completeTxs = Cons source.tx source.completeTxs
  }
