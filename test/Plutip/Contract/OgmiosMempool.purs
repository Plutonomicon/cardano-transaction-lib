module Test.Ctl.Plutip.Contract.OgmiosMempool
  ( suite
  ) where

import Prelude

import Contract.Backend.Ogmios.Mempool (acquireMempoolSnapshot, fetchMempoolTxs)
import Contract.Scripts (validatorHash)
import Contract.Test (ContractTest, InitialUTxOs, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Ctl.Examples.PlutusV2.InlineDatum as InlineDatum
import Data.BigInt as BigInt
import Effect.Class (liftEffect)
import Effect.Console as Console
import Mote (group, test)

suite :: TestPlanM ContractTest Unit
suite = group "Ogmios mempool test" do
  test "acquireMempoolSnapshot" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ BigInt.fromInt 1000_000_000
        , BigInt.fromInt 2000_000_000
        ]
    withWallets distribution \alice -> do
      withKeyWallet alice do
        liftEffect <<< Console.log <<< show =<< acquireMempoolSnapshot
  test "fetchMempoolTXs" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ BigInt.fromInt 1000_000_000
        , BigInt.fromInt 2000_000_000
        ]
    withWallets distribution \alice -> do
      withKeyWallet alice do
        validator <- InlineDatum.checkDatumIsInlineScript
        let vhash = validatorHash validator
        void $ InlineDatum.payToCheckDatumIsInline vhash
        mpTxs <- fetchMempoolTxs =<< acquireMempoolSnapshot
        liftEffect <<< Console.log <<< show $ mpTxs
        pure unit
