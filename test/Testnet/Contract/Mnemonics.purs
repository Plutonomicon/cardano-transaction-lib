module Test.Ctl.Testnet.Contract.Mnemonics (suite) where

import Prelude

import Contract.Address (addressFromBech32)
import Contract.Test (ContractTest, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Wallet (getWalletAddresses, withKeyWalletFromMnemonic)
import Contract.Wallet.Key (StakeKeyPresence(WithStakeKey))
import Data.Array (head)
import Data.UInt as UInt
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM ContractTest Unit
suite = do
  group "CIP-1852 mnemonics" do
    test "Allows to build multiple KeyWallets" do
      withWallets unit \_ -> do
        do
          addr1 <- withKeyWalletFromMnemonic
            phrase1
            { accountIndex: UInt.fromInt 0
            , addressIndex: UInt.fromInt 0
            }
            WithStakeKey
            do
              head <$> getWalletAddresses
          addrExpected <- addressFromBech32
            "addr_test1qrday0u0gtx3302u5mmgmw20q67s9mkglte8y8kqk75jge5mvvhnhsjfj5jfpt7dv4tu6wlz7z032cmmp9ljftjmkzfsdfxge2"
          addr1 `shouldEqual` pure addrExpected
        do
          addr1 <- withKeyWalletFromMnemonic
            phrase1
            { accountIndex: UInt.fromInt 1
            , addressIndex: UInt.fromInt 1
            }
            WithStakeKey
            do
              head <$> getWalletAddresses
          addrExpected <- addressFromBech32
            "addr_test1qpmmg8l2w7ar4cj89jte699dxuu0u3partkjt7fqdga5a7u6w4gzwu7mms8sn7rd0apcwtyu2xjzflvm7sc5vd3vtvpqs2m3xz"
          addr1 `shouldEqual` pure addrExpected

  where
  phrase1 =
    "what abstract myself forum setup leader series maximum home abuse shadow wreck inflict dust basket cycle involve quick abstract eagle staff town voyage raven"
