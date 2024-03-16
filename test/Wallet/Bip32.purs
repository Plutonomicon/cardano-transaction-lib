module Test.Ctl.Wallet.Bip32
  ( suite
  ) where

import Contract.Prelude

import Cardano.Types.Address as Address
import Cardano.Types.NetworkId (NetworkId(MainnetId))
import Contract.Wallet.Key
  ( StakeKeyPresence(WithStakeKey)
  , mkKeyWalletFromMnemonic
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Wallet.Key (KeyWallet(KeyWallet))
import Data.Lens (_Left, preview)
import Data.UInt as UInt
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "BIP32/BIP39/CIP1852 wallets" do
    group "generates valid wallets for valid phrases" do
      for_ accs \(accountIndex /\ addressIndex /\ addressStr) -> do
        test
          ( "Account index produces correct wallet (m/1852'/1815'/"
              <> show accountIndex
              <> "'/"
              <> show addressIndex
              <> ")"
          )
          do
            Address.fromBech32 addressStr `shouldEqual`
              hush
                ( mkKeyWalletFromMnemonic phrase1
                    { accountIndex: UInt.fromInt accountIndex
                    , addressIndex: UInt.fromInt addressIndex
                    }
                    WithStakeKey <#>
                    \(KeyWallet wallet) -> wallet.address MainnetId
                )
  group "Invalid mnemonics" do
    test "handles errors for invalid phrases" do
      blush (mkKeyWalletFromMnemonic invalidPhrase zero WithStakeKey)
        `shouldEqual` Just "Error: Invalid mnemonic"
      blush (mkKeyWalletFromMnemonic invalidChecksum zero WithStakeKey)
        `shouldEqual` Just "Error: Invalid mnemonic checksum"
  where
  blush = preview _Left
  invalidPhrase =
    "twelve twelve twelve twelve twelve twelve twelve twelve twelve twelve twelve"
  invalidChecksum =
    "twelve twelve twelve twelve twelve twelve twelve twelve twelve twelve twelve zebra"
  phrase1 =
    "what abstract myself forum setup leader series maximum home abuse shadow wreck inflict dust basket cycle involve quick abstract eagle staff town voyage raven"
  -- these fixtures are from Eternl wallet
  accs =
    [ 0 /\ 0 /\
        "addr1q8day0u0gtx3302u5mmgmw20q67s9mkglte8y8kqk75jge5mvvhnhsjfj5jfpt7dv4tu6wlz7z032cmmp9ljftjmkzfswlmg44"
    , 0 /\ 1 /\
        "addr1qyp2qjzpq6mg3yd4uxnfl40pr7z7l9fl64tuuce45e00ejumvvhnhsjfj5jfpt7dv4tu6wlz7z032cmmp9ljftjmkzfszpjxxa"
    , 0 /\ 2 /\
        "addr1q94zxqrq57c8j05gdz2vq06gdejxlngpyezeyxask8w5j9umvvhnhsjfj5jfpt7dv4tu6wlz7z032cmmp9ljftjmkzfskqdfjm"
    , 0 /\ 3 /\
        "addr1q8rpcfpamcdw3ldhz65st3sh2artdv0vtpqafelz9a0nt3umvvhnhsjfj5jfpt7dv4tu6wlz7z032cmmp9ljftjmkzfs7zugcf"
    , 0 /\ 4 /\
        "addr1qyn58rgkzd9jlw8ay7hz34lwdaa0u8l2ww7qkxp2s08y88ymvvhnhsjfj5jfpt7dv4tu6wlz7z032cmmp9ljftjmkzfsx84jh7"
    , 1 /\ 0 /\
        "addr1qyka9awkxtcm4py0yf9qnzdeh786zjqp6lt37purjrxtaju6w4gzwu7mms8sn7rd0apcwtyu2xjzflvm7sc5vd3vtvpq4xgxpq"
    , 1 /\ 1 /\
        "addr1q9mmg8l2w7ar4cj89jte699dxuu0u3partkjt7fqdga5a7u6w4gzwu7mms8sn7rd0apcwtyu2xjzflvm7sc5vd3vtvpqnux32a"
    , 1 /\ 2 /\
        "addr1q8eddt79d4nt3hazvjdwa3cy27nukxnvv0j5x7g28atn3ey6w4gzwu7mms8sn7rd0apcwtyu2xjzflvm7sc5vd3vtvpq66x2a8"
    ]
