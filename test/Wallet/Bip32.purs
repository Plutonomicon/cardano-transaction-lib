module Test.Ctl.Wallet.Bip32
  ( suite
  ) where

import Contract.Prelude

import Contract.Wallet (mkKeyWalletFromMnemonic)
import Ctl.Internal.Serialization.Address
  ( NetworkId(MainnetId)
  , addressFromBech32
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Wallet.Key (KeyWallet(KeyWallet))
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Ctl.Utils (assertTrue)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "BIP32/BIP39/CIP1852 wallets" do
    test "generates valid wallets for valid phrases" do
      assertTrue "Valid phrase produces correct wallet" $
        case mkKeyWalletFromMnemonic goodPhrase zero of
          Left _ -> false
          Right (KeyWallet wallet) ->
            Just (wallet.address MainnetId) == addressFromBech32
              goodPhraseAddress0
      assertTrue "Account index produces correct wallet" $
        case mkKeyWalletFromMnemonic goodPhrase one of
          Left _ -> false
          Right (KeyWallet wallet) ->
            Just (wallet.address MainnetId) == addressFromBech32
              goodPhraseAddress1
    test "handles errors for invalid phrases" do
      assertTrue "Invalid phrase length returns an error" $
        case mkKeyWalletFromMnemonic invalidPhrase zero of
          Left e -> e == "Error: Invalid mnemonic"
          Right _ -> false
      assertTrue "Invalid phrase checksum returns an error" $
        case mkKeyWalletFromMnemonic invalidChecksum zero of
          Left e -> e == "Error: Invalid mnemonic checksum"
          Right _ -> false
  where
  goodPhrase =
    "twelve twelve twelve twelve twelve twelve twelve twelve twelve twelve twelve twelve"
  goodPhraseAddress0 =
    "addr1q9w8atxfen0fqd99rnagywam68n7tvzg4g5t39azwtgm2uy0cwl6t7cd6sffn3l8ryyfn4ztw009gqtumhenxahughnspt8frx"
  goodPhraseAddress1 =
    "addr1q85w3avkx4xvsadj57sw4eumrl8tcsq6yhr3evv4zy0gavhmyjhs96g96jax70qqyskmwrzvc6jm45w4y76q29vful2snqc60k"
  invalidPhrase =
    "twelve twelve twelve twelve twelve twelve twelve twelve twelve twelve twelve"
  invalidChecksum =
    "twelve twelve twelve twelve twelve twelve twelve twelve twelve twelve twelve zebra"
