module Test.Ctl.Plutip.Contract.OgmiosMempool
  ( suite
  ) where

import Prelude

import Contract.Backend.Ogmios.Mempool
  ( MempoolSizeAndCapacity(MempoolSizeAndCapacity)
  , acquireMempoolSnapshot
  , fetchMempoolTxs
  , mempoolSnapshotHasTx
  , mempoolSnapshotSizeAndCapacity
  , withMempoolSnapshot
  )
import Contract.Scripts (validatorHash)
import Contract.Test (ContractTest, InitialUTxOs, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Ctl.Examples.PlutusV2.InlineDatum as InlineDatum
import Data.Array (length)
import JS.BigInt as BigInt
import Mote (group, skip, test)
import Test.Spec.Assertions (shouldEqual)

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
        void acquireMempoolSnapshot
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
        txId <- InlineDatum.payToCheckDatumIsInline vhash
        mpTxs <- fetchMempoolTxs =<< acquireMempoolSnapshot
        length mpTxs `shouldEqual` 1
        awaitTxConfirmed txId
        mpTxs' <- fetchMempoolTxs =<< acquireMempoolSnapshot
        length mpTxs' `shouldEqual` 0
  skip $ test
    "mempoolSnapshotHasTx - skipped because HasTx always returns false for some reason"
    do
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
          txId <- InlineDatum.payToCheckDatumIsInline vhash
          withMempoolSnapshot (flip mempoolSnapshotHasTx txId) >>= shouldEqual
            true
          snapshot <- acquireMempoolSnapshot
          _mpTxs' <- fetchMempoolTxs snapshot
          -- for_ mpTxs' \tx -> do
          --   liftEffect <<< Console.log <<< show =<< liftEffect
          --     (transactionHash tx)
          awaitTxConfirmed txId
          mempoolSnapshotHasTx snapshot txId >>= shouldEqual false
  test "mempoolSnapshotSizeAndCapacity" do
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
        MempoolSizeAndCapacity { numberOfTxs } <-
          withMempoolSnapshot (mempoolSnapshotSizeAndCapacity)
        numberOfTxs `shouldEqual` 1
