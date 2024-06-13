module Test.Ctl.Testnet.DistributeFunds where

import Contract.Prelude

import Contract.Test.Mote (TestPlanM)
import Ctl.Internal.Testnet.DistributeFunds as Distribute
import Data.Bifunctor (lmap)
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

    -- emptyTx :: Distribute.Tx Unit String Int
    -- emptyTx =
    --   { source: { key: unit }
    --   , total: 0
    --   , totalUtxos: 0
    --   , utxos: Nil
    --   }

    -- emptySource :: Int -> Distribute.SourceState Int String Int
    -- emptySource src =
    --   { source: src
    --   , leftover: 0
    --   , tx: emptyTx
    --   , completeTxs: Nil
    --   }
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
          ( Distribute.initialSourceState
              { key: 0, initialFunds: 90 }
          )
            { tx = (Distribute.emptyTx 370)
                { totalUtxos = 2
                , utxos = List.fromFoldable
                    [ { key: "tgt0", amount: 300 }
                    , { key: "tgt42", amount: 70 }
                    ]
                }
            }

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
              { leftover = src1.leftover - utxo1.amount
              , tx = (Distribute.emptyTx utxo1.amount)
                  { totalUtxos = 1
                  , utxos = Cons utxo1 Nil
                  }
              }
          , src0
              { leftover = src0.leftover - utxo0.amount
              , tx = Distribute.emptyTx zero
              , completeTxs =
                  let
                    tx' = src0.tx
                      { total = src0.tx.total + utxo0.amount
                      , totalUtxos = 3
                      , utxos = Cons utxo0 src0.tx.utxos
                      }
                  in
                    Cons tx' src0.completeTxs
              }
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
              { leftover = 0
              -- it would be put in completeTxs on the next iteration
              , tx = (Distribute.emptyTx 20)
                  { totalUtxos = 2
                  , utxos = List.reverse utxos3
                  }
              , completeTxs = List.fromFoldable
                  [ (Distribute.emptyTx 20)
                      { totalUtxos = 2
                      , utxos = List.reverse utxos1
                      }
                  ]
              }
          , src0
              { leftover = 0
              , completeTxs = List.fromFoldable
                  [ (Distribute.emptyTx 20)
                      { totalUtxos = 2
                      , utxos = List.reverse utxos2
                      }
                  , (Distribute.emptyTx 20)
                      { totalUtxos = 2
                      , utxos = List.reverse utxos0
                      }
                  ]

              }
          ]
      outcome `shouldEqual` Right expected

    -- It could be better: seach for a source that have a Tx with lowest total or lowest amount of UTxOs
    -- But if it worths it?
    test "Makes new Tx if utxo is impossible to fit in existing ones" do
      let
        src0 =
          ( Distribute.initialSourceState
              { key: 0, initialFunds: 900 }
          )
            { tx = Distribute.emptyTx 120
            }
        src1 =
          ( Distribute.initialSourceState
              { key: 1, initialFunds: 800 }
          )
            { tx = Distribute.emptyTx 105
            }
        utxo = { key: "utxo0", amount: 100 }
        outcome =
          Distribute.assignUtxo
            highThreshold { maxCoinPerTx = 200 }
            utxo
            $ List.fromFoldable [ src0, src1 ]
        expected = List.fromFoldable
          [ src0
          , src1
              { leftover = src1.leftover - utxo.amount
              , tx = (Distribute.emptyTx utxo.amount)
                  { totalUtxos = 1
                  , utxos = Cons utxo Nil
                  }
              , completeTxs = Cons src1.tx Nil
              }
          ]
      outcome `shouldEqual` Right expected
    test "Tries to fit UTxO in any constructing tx that can fit it" do
      let
        src0 =
          ( Distribute.initialSourceState
              { key: 0, initialFunds: 900 }
          )
            -- not enough to fit the utxo
            { tx = Distribute.emptyTx 120
            }
        src1 =
          ( Distribute.initialSourceState
              { key: 1, initialFunds: 800 }
          )
            { -- exactly enough to fit the utxo
              tx = Distribute.emptyTx 100
            }
        utxo = { key: "utxo0", amount: 100 }
        outcome =
          Distribute.assignUtxo
            highThreshold { maxCoinPerTx = 200 }
            utxo
            $ List.fromFoldable [ src0, src1 ]
        expected = List.fromFoldable
          [ src0
          , src1
              { leftover = src1.leftover - utxo.amount
              , tx =
                  ( Distribute.emptyTx
                      (src1.tx.total + utxo.amount)
                  )
                    { totalUtxos = 1
                    , utxos = Cons utxo Nil
                    }
              }
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
        outcome = map (List.sortBy $ comparing _.source)
          $ Distribute.assignUtxo highThreshold utxo
          $ List.fromFoldable [ src0, src1, src2 ]
        expected = List.sortBy (comparing _.source) $ List.fromFoldable
          [ src0
          , src1
          , src2
              { leftover = src2.leftover - utxo.amount
              , tx = (Distribute.emptyTx utxo.amount)
                  { totalUtxos = 1
                  , utxos = Cons utxo Nil
                  }
              }
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

