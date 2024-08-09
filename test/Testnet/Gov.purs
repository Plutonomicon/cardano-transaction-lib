module Test.Ctl.Testnet.Gov
  ( suite
  ) where

import Prelude

import Cardano.Types.BigNum (fromInt) as BigNum
import Contract.Test (ContractTest)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Testnet (withKeyWallet, withWallets)
import Ctl.Examples.Gov.DelegateVoteAbstain (contract) as Gov.DelegateVoteAbstain
import Ctl.Examples.Gov.ManageDrep (contract) as Gov.ManageDrep
import Ctl.Examples.Gov.ManageDrepScript (contract) as Gov.ManageDrepScript
import Ctl.Examples.Gov.SubmitVote (contract) as Gov.SubmitVote
import Ctl.Examples.Gov.SubmitVoteScript (contract) as Gov.SubmitVoteScript
import Ctl.Internal.Test.UtxoDistribution (TestWalletSpec)
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import Mote (group, test)
import Test.Ctl.Testnet.Common (privateDrepKey, privateStakeKey)

walletSpec :: TestWalletSpec
walletSpec = wrap
  { utxos:
      [ BigNum.fromInt 1_000_000_000
      , BigNum.fromInt 50_000_000
      ]
  , stakeKey: Just privateStakeKey
  , drepKey: Just privateDrepKey
  }

suite :: TestPlanM ContractTest Unit
suite = do
  group "Governance" do
    test "Gov.DelegateVoteAbstain" do
      withWallets walletSpec \alice ->
        withKeyWallet alice Gov.DelegateVoteAbstain.contract

    test "Gov.ManageDrep example" do
      withWallets walletSpec \alice ->
        withKeyWallet alice Gov.ManageDrep.contract

    test "Gov.ManageDrepScript example" do
      withWallets walletSpec \alice ->
        withKeyWallet alice Gov.ManageDrepScript.contract

    test "Gov.SubmitVote example" do
      withWallets walletSpec \alice ->
        withKeyWallet alice Gov.SubmitVote.contract

    test "Gov.SubmitVoteScript example" do
      withWallets walletSpec \alice ->
        withKeyWallet alice Gov.SubmitVoteScript.contract
