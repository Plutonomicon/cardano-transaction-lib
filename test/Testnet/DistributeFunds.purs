module Test.Ctl.Testnet.DistributeFunds where

import Contract.Prelude hiding (over)

import Contract.Test.Mote (TestPlanM)
import Ctl.Internal.Testnet.DistributeFunds
  ( _completeTxs
  , _leftover
  , _source
  , _total
  , _totalUtxos
  , _tx
  , _utxos
  )
import Ctl.Internal.Testnet.DistributeFunds as Distribute
import Data.Bifunctor (lmap)
import Data.Lens (over, set, view, (%~), (+~), (-~), (.~), (^.))
import Data.List (List(..))
import Data.List as List
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = group "Testnet" $ group "Distribute Funds" do
  group "assignUtxo" do
    let
      highThreshold =
        { maxCoinPerTx: 999_999
        , maxTargetUtxosPerTx: 999_999
        }

    test "Fails if sources do not have enough funds" do

      let
        outcome =
          Distribute.assignUtxo
            highThreshold
            { amount: 120, key: "utxo0" }
            $ List.fromFoldable
                [ Distribute.initialSourceState
                    { key: 1, initialFunds: 90 }
                , Distribute.initialSourceState
                    { key: 2, initialFunds: 100 }
                ]
      lmap (const unit) outcome `shouldEqual` Left unit
    test "Starts new Tx when reaches the limit of UTxOs" do
      let
        thresholds = highThreshold { maxTargetUtxosPerTx = 3 }

        src0 :: Distribute.SourceState Int String Int
        src0 =
          Distribute.initialSourceState
            { key: 0, initialFunds: 90 }
            # (_tx <<< _total .~ 370)
            # (_tx <<< _totalUtxos .~ 2)
            #
              ( _tx <<< _utxos .~ List.fromFoldable
                  [ { key: "tgt0", amount: 300 }
                  , { key: "tgt42", amount: 70 }
                  ]
              )

        src1 :: Distribute.SourceState Int String Int
        src1 = Distribute.initialSourceState
          { key: 1, initialFunds: 30 }

        utxo0 = { amount: 10, key: "utxo0" }
        utxo1 = { amount: 6, key: "utxo1" }
        acc0 = List.fromFoldable [ src0, src1 ]
        outcome = do
          acc1 <- Distribute.assignUtxo
            thresholds
            utxo0
            acc0
          acc2 <- Distribute.assignUtxo
            thresholds
            utxo1
            acc1
          pure acc2

        expected :: List (Distribute.SourceState Int String Int)
        expected = List.fromFoldable
          [ src1
              # (_leftover -~ utxo1.amount)
              #
                ( set _tx
                    $ Distribute.emptyTx utxo1.amount
                    # (_totalUtxos .~ 1)
                    # (_utxos .~ pure utxo1)
                )
          , src0
              # (_leftover -~ utxo0.amount)
              # (_tx .~ Distribute.emptyTx zero)
              #
                ( over _completeTxs
                    $ src0
                    # (view _tx)
                    # (_total +~ utxo0.amount)
                    # (_totalUtxos .~ 3)
                    # (_utxos %~ Cons utxo0)
                    # Cons
                )
          ]

      outcome `shouldEqual` Right expected
    test "Tends to spend sources evenly" do
      let
        utxos0 = List.fromFoldable
          [ { key: "01", amount: 2 }
          , { key: "02", amount: 18 }
          ]
        utxos1 = List.fromFoldable
          [ { key: "11", amount: 3 }
          , { key: "12", amount: 17 }
          ]
        utxos2 = List.fromFoldable
          [ { key: "21", amount: 9 }
          , { key: "22", amount: 11 }
          ]
        utxos3 = List.fromFoldable
          [ { key: "31", amount: 15 }
          , { key: "32", amount: 5 }
          ]
        -- total = 80
        utxos = utxos0 <> utxos1 <> utxos2 <> utxos3

        -- they have exactly enough to fit all the utxos
        src0 = Distribute.initialSourceState
          { key: 0, initialFunds: 40 }
        src1 = Distribute.initialSourceState
          { key: 1, initialFunds: 40 }
        sources = List.fromFoldable [ src0, src1 ]

        outcome = foldM
          ( flip $ Distribute.assignUtxo highThreshold
              { maxTargetUtxosPerTx = 2 }
          )
          sources
          utxos

        -- Both must have 2 txs, 20 UTxO each. All sources funds must be spent.
        expected = List.fromFoldable
          [ src1
              # (_leftover .~ 0)
              -- it would be put in completeTxs on the next iteration
              #
                ( set _tx
                    $ Distribute.emptyTx 20
                    # (_totalUtxos .~ 2)
                    # (_utxos .~ List.reverse utxos3)
                )
              #
                ( set _completeTxs
                    $ Distribute.emptyTx 20
                    # (_totalUtxos .~ 2)
                    # (_utxos .~ List.reverse utxos1)
                    # pure
                )
          , src0
              # (_leftover .~ 0)
              #
                ( _completeTxs .~ List.fromFoldable
                    [ Distribute.emptyTx 20
                        # (_totalUtxos .~ 2)
                        # (_utxos .~ List.reverse utxos2)
                    , Distribute.emptyTx 20
                        # (_totalUtxos .~ 2)
                        # (_utxos .~ List.reverse utxos0)
                    ]
                )
          ]
      outcome `shouldEqual` Right expected

    -- It could be better: seach for a source that have a Tx with lowest total or lowest amount of UTxOs
    -- But if it worths it?
    test "Makes new Tx if utxo is impossible to fit in existing ones" do
      let
        src0 =
          Distribute.initialSourceState
            { key: 0, initialFunds: 900 }
            # (_tx <<< _total .~ 120)
        src1 =
          Distribute.initialSourceState
            { key: 1, initialFunds: 800 }
            # (_tx <<< _total .~ 105)
        utxo = { key: "utxo0", amount: 100 }
        outcome =
          Distribute.assignUtxo
            highThreshold { maxCoinPerTx = 200 }
            utxo
            $ List.fromFoldable [ src0, src1 ]
        expected = List.fromFoldable
          [ src0
          , src1
              # (_leftover -~ utxo.amount)
              #
                ( set _tx
                    $ Distribute.emptyTx utxo.amount
                    # (_totalUtxos .~ 1)
                    # (_utxos .~ pure utxo)
                )
              # (_completeTxs .~ pure (src1 ^. _tx))
          ]
      outcome `shouldEqual` Right expected
    test "Tries to fit UTxO in any constructing tx that can fit it" do
      let
        src0 =
          Distribute.initialSourceState
            { key: 0, initialFunds: 900 }
            -- not enough to fit the utxo
            # (_tx <<< _total .~ 120)
        src1 =
          Distribute.initialSourceState
            { key: 1, initialFunds: 800 }
            -- exactly enough to fit the utxo
            # (_tx <<< _total .~ 100)
        utxo = { key: "utxo0", amount: 100 }
        outcome =
          Distribute.assignUtxo
            highThreshold { maxCoinPerTx = 200 }
            utxo
            $ List.fromFoldable [ src0, src1 ]
        expected = List.fromFoldable
          [ src0
          , src1
              # (_leftover -~ utxo.amount)
              # (_tx <<< _total +~ utxo.amount)
              # (_tx <<< _totalUtxos +~ 1)
              # (_tx <<< _utxos .~ pure utxo)
          ]
      outcome `shouldEqual` Right expected
    test "Tries to fit UTxO in any source tx that has enough funds" do
      let
        -- not enough
        src0 = Distribute.initialSourceState
          { key: 0, initialFunds: 200 }
        src1 = Distribute.initialSourceState
          { key: 1, initialFunds: 100 }
        -- enough
        src2 = Distribute.initialSourceState
          { key: 2, initialFunds: 300 }
        utxo = { key: "utxo0", amount: 250 }
        outcome = map (List.sortBy $ comparing (view _source))
          $ Distribute.assignUtxo highThreshold utxo
          $ List.fromFoldable [ src0, src1, src2 ]
        expected = List.sortBy (comparing $ view _source) $ List.fromFoldable
          [ src0
          , src1
          , src2
              # (_leftover -~ utxo.amount)
              #
                ( set _tx
                    $ Distribute.emptyTx utxo.amount
                    # (_totalUtxos .~ 1)
                    # (_utxos .~ pure utxo)
                )
          ]
      outcome `shouldEqual` Right expected
    test "Fails if UTxO amount is higher than threshold" do
      let
        -- not enough
        src0 = Distribute.initialSourceState
          { key: 0, initialFunds: 300 }
        src1 = Distribute.initialSourceState
          { key: 1, initialFunds: 900 }
        utxo = { key: "utxo0", amount: 250 }
        outcome =
          Distribute.assignUtxo
            highThreshold { maxCoinPerTx = 200 }
            utxo
            $ List.fromFoldable [ src0, src1 ]
      lmap (const unit) outcome `shouldEqual` Left unit
  pure unit

