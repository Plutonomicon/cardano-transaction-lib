module Test.Ctl.Testnet.DistributeFunds
  ( suite
  ) where

import Prelude

import Contract.Test.Mote (TestPlanM)
import Ctl.Internal.Testnet.DistributeFunds
  ( AssignUtxoResult
      ( AssignUtxo_Unassigned
      , AssignUtxo_Deferred
      , AssignUtxo_AssignedToSource
      )
  , DistrFundsError(DistrFunds_AssignUtxoError)
  , DistrFundsParams
  , SourceState
  , assignUtxoToSource
  , initSourceState
  , makeDistributionPlan
  , runDistrFundsRound
  )
import Data.Array (reverse)
import Data.Either (Either(Left, Right))
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (modify, wrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "DistributeFunds" do
    group "assignUtxoToSource" do
      test "Leaves utxo unassigned if it cannot be covered by source"
        let
          src0 = initSourceState "src0" 1000
          utxo = "target0" /\ 2000
        in
          assignUtxoToSource defaultParams src0 utxo `shouldEqual`
            AssignUtxo_Unassigned

      test "Takes tx fee into account"
        let
          params = defaultParams { feePerTx = 10 }
          src0 = initSourceState "src0" 1000
          utxo = "target0" /\ 1000
        in
          assignUtxoToSource params src0 utxo `shouldEqual`
            AssignUtxo_Unassigned

      test "Marks utxo as deferred if maxUtxosPerTx is exceeded"
        let
          params = defaultParams { maxUtxosPerTx = 0 }
          src0 = initSourceState "src0" 1000
          utxo = "target0" /\ 1000
        in
          assignUtxoToSource params src0 utxo `shouldEqual`
            AssignUtxo_Deferred

      test "Correctly assigns utxos to source"
        let
          params = defaultParams { feePerTx = 100 }
          src0 = initSourceState "src0" 2000
          utxo0 = "target0" /\ 1000
          utxo1 = "target1" /\ 500
          assignUtxo = flip (assignUtxoToSource params)
          outcome =
            (getSource <<< assignUtxo utxo1)
              =<< getSource (assignUtxo utxo0 src0)
        in
          outcome `shouldEqual` Just
            ( src0
                { leftover = 500
                , currentTx = modify
                    ( _
                        { numUtxos = 2
                        , utxos = List.fromFoldable
                            [ utxoToRec utxo1, utxoToRec utxo0 ]
                        }
                    )
                    src0.currentTx
                }
            )

    group "runDistrFundsRound" do
      test "Fails if utxo cannot be covered by any source"
        let
          sources = List.fromFoldable
            [ initSourceState "src0" 1000
            , initSourceState "src1" 2000
            , initSourceState "src2" 3000
            ]
          utxos = List.fromFoldable
            [ "target0" /\ 1000
            , "target1" /\ 3500
            ]
        in
          runDistrFundsRound defaultParams sources utxos `shouldSatisfy`
            case _ of
              Left (DistrFunds_AssignUtxoError _) -> true
              _ -> false

      test "Deferrs utxos that cannot be assigned in the current round"
        let
          params = defaultParams { maxUtxosPerTx = 1 }
          src0 = initSourceState "src0" 2000
          src1 = initSourceState "src1" 1000
          sources = List.fromFoldable [ src0, src1 ]
          utxo0 = "target0" /\ 1600
          utxo1 = "target1" /\ 800
          utxo2 = "target2" /\ 400
          utxos = List.fromFoldable [ utxo0, utxo1, utxo2 ]
        in
          runDistrFundsRound params sources utxos `shouldEqual`
            Right
              { sources: List.fromFoldable
                  [ src0
                      { leftover = 400
                      , currentTx = modify
                          ( _
                              { numUtxos = 1
                              , utxos = List.fromFoldable [ utxoToRec utxo0 ]
                              }
                          )
                          src0.currentTx
                      }
                  , src1
                      { leftover = 200
                      , currentTx = modify
                          ( _
                              { numUtxos = 1
                              , utxos = List.fromFoldable [ utxoToRec utxo1 ]
                              }
                          )
                          src1.currentTx
                      }
                  ]
              , deferredTargets: List.fromFoldable [ utxo2 ]
              }

    group "makeDistributionPlan" do
      test "Prepares simple funds distribution plan (2 rounds)"
        let
          params = defaultParams { maxUtxosPerTx = 2, maxRounds = 5 }

          utxos0 = [ "01" /\ 2, "02" /\ 18 ]
          utxos1 = [ "11" /\ 3, "12" /\ 17 ]
          utxos2 = [ "21" /\ 9, "22" /\ 11 ]
          utxos3 = [ "31" /\ 15, "32" /\ 5 ]

          -- total = 80
          utxos = utxos0 <> utxos1 <> utxos2 <> utxos3

          -- sources have exactly enough funds to fit all the utxos
          src0 = "src0" /\ 40
          src1 = "src1" /\ 40
          sources = [ src0, src1 ]
        in
          makeDistributionPlan params sources utxos `shouldEqual`
            Right
              [ [ wrap
                    { srcWallet: "src0"
                    , numUtxos: 2
                    , utxos: List.fromFoldable $ utxoToRec <$> reverse utxos1
                    }
                , wrap
                    { srcWallet: "src1"
                    , numUtxos: 2
                    , utxos: List.fromFoldable $ utxoToRec <$> reverse utxos0
                    }
                ]
              , [ wrap
                    { srcWallet: "src0"
                    , numUtxos: 2
                    , utxos: List.fromFoldable $ utxoToRec <$> utxos2
                    }
                , wrap
                    { srcWallet: "src1"
                    , numUtxos: 2
                    , utxos: List.fromFoldable $ utxoToRec <$> utxos3
                    }
                ]
              ]

utxoToRec
  :: forall wallet amount
   . wallet /\ amount
  -> { wallet :: wallet, amount :: amount }
utxoToRec (wallet /\ amount) = { wallet, amount }

getSource
  :: forall wallet amount
   . AssignUtxoResult wallet amount
  -> Maybe (SourceState wallet amount)
getSource = case _ of
  AssignUtxo_AssignedToSource src -> Just src
  _ -> Nothing

defaultParams :: forall wallet. DistrFundsParams wallet Int
defaultParams =
  { maxRounds: top
  , maxUtxosPerTx: top
  , getUtxoMinAdaForWallet: const zero
  , feePerTx: zero
  }
