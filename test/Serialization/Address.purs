module Test.Serialization.Address (suite) where

import Prelude

import Ctl.Internal.Test.Utils (errMaybe)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Mote (group, test)
import Serialization.Address
  ( NetworkId(MainnetId, TestnetId)
  , addressBech32
  , addressBytes
  , addressFromBech32
  , addressFromBytes
  , addressNetworkId
  , baseAddressDelegationCred
  , baseAddressFromAddress
  , baseAddressPaymentCred
  , baseAddressToAddress
  , enterpriseAddress
  , enterpriseAddressFromAddress
  , enterpriseAddressPaymentCred
  , enterpriseAddressToAddress
  , keyHashCredential
  , paymentKeyHashStakeKeyHashAddress
  , pointerAddress
  , pointerAddressFromAddress
  , pointerAddressPaymentCred
  , pointerAddressStakePointer
  , pointerAddressToAddress
  , rewardAddress
  , rewardAddressFromAddress
  , rewardAddressPaymentCred
  , rewardAddressToAddress
  , scriptHashCredential
  , stakeCredentialFromBytes
  , stakeCredentialToBytes
  , stakeCredentialToKeyHash
  , stakeCredentialToScriptHash
  )
import Serialization.Hash
  ( Ed25519KeyHash
  , ScriptHash
  , ed25519KeyHashFromBech32
  , scriptHashFromBytes
  )
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Types.Aliases (Bech32String)
import Types.BigNum (fromInt, fromStringUnsafe) as BigNum
import Types.RawBytes (hexToRawBytesUnsafe)
import Test.Fixtures (ed25519KeyHashFixture1)

doesNotThrow
  :: forall (f :: Type -> Type) (a :: Type). Applicative f => a -> f a
doesNotThrow = pure

pkhBech32 :: Bech32String
pkhBech32 = "addr_vkh1zuctrdcq6ctd29242w8g84nlz0q38t2lnv3zzfcrfqktx0c9tzp"

scriptHashHex :: String
scriptHashHex = "463e9264962cea64efafa576d44c8d2821d09c0c7495c253d4101a9a"

mPkh :: Maybe Ed25519KeyHash
mPkh = ed25519KeyHashFromBech32 pkhBech32

mScriptHash :: Maybe ScriptHash
mScriptHash = scriptHashFromBytes $ hexToRawBytesUnsafe scriptHashHex

addressFunctionsTest :: TestPlanM (Aff Unit) Unit
addressFunctionsTest = test "Address tests" $ do
  let
    bechstr =
      "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"
  addr1 <- errMaybe "addressFromBech32 failed on valid bech32" $
    addressFromBech32 bechstr
  bechstr `shouldEqual` addressBech32 addr1
  addressFromBech32 "randomstuff" `shouldEqual` Nothing
  let addrBts = addressBytes addr1
  addr2 <- errMaybe "addressFromBech32 failed on valid bech32" $
    addressFromBytes addrBts
  addr2 `shouldEqual` addr1
  addressNetworkId addr2 `shouldEqual` MainnetId

stakeCredentialTests :: TestPlanM (Aff Unit) Unit
stakeCredentialTests = test "StakeCredential tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  scrh <- errMaybe "Error scriptHashFromBech32:" mScriptHash

  let
    pkhCred = keyHashCredential $ pkh
    schCred = scriptHashCredential $ scrh
    pkhCredBytes = stakeCredentialToBytes pkhCred
    schCredBytes = stakeCredentialToBytes schCred

  pkhCred2 <- errMaybe "stakeCredentialFromBytes failed on valid bytes" $
    stakeCredentialFromBytes
      pkhCredBytes
  pkh2 <- errMaybe "stakeCredentialToKeyHash failed" $ stakeCredentialToKeyHash
    pkhCred2
  pkh2 `shouldEqual` pkh
  stakeCredentialToScriptHash pkhCred2 `shouldEqual` Nothing

  schCred2 <- errMaybe "takeCredentialFromBytes failed on valid bytes" $
    stakeCredentialFromBytes
      schCredBytes
  sch2 <- errMaybe "stakeCredentialToScriptHash failed" $
    stakeCredentialToScriptHash schCred2
  sch2 `shouldEqual` scrh
  stakeCredentialToKeyHash schCred2 `shouldEqual` Nothing

baseAddressFunctionsTest :: TestPlanM (Aff Unit) Unit
baseAddressFunctionsTest = test "BaseAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  baddr <- doesNotThrow $
    paymentKeyHashStakeKeyHashAddress MainnetId pkh ed25519KeyHashFixture1
  addr <- doesNotThrow $ baseAddressToAddress baddr
  baddr2 <- errMaybe "baseAddressFromAddress failed on valid base address" $
    baseAddressFromAddress
      addr
  baddr2 `shouldEqual` baddr
  baseAddressDelegationCred baddr `shouldEqual` keyHashCredential
    ed25519KeyHashFixture1
  baseAddressPaymentCred baddr `shouldEqual` keyHashCredential pkh

rewardAddressFunctionsTest :: TestPlanM (Aff Unit) Unit
rewardAddressFunctionsTest = test "RewardAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  raddr <- doesNotThrow $ rewardAddress
    { network: TestnetId, paymentCred: keyHashCredential pkh }
  addr <- doesNotThrow $ rewardAddressToAddress raddr
  raddr2 <- errMaybe "rewardAddressFromAddress failed on valid reward address" $
    rewardAddressFromAddress addr
  raddr2 `shouldEqual` raddr
  rewardAddressPaymentCred raddr `shouldEqual` keyHashCredential pkh

enterpriseAddressFunctionsTest :: TestPlanM (Aff Unit) Unit
enterpriseAddressFunctionsTest = test "EnterpriseAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  eaddr <- doesNotThrow $ enterpriseAddress
    { network: MainnetId, paymentCred: keyHashCredential pkh }
  addr <- doesNotThrow $ enterpriseAddressToAddress eaddr
  eaddr2 <-
    errMaybe "enterpriseAddressFromAddress failed on valid enterprise address" $
      enterpriseAddressFromAddress addr
  eaddr2 `shouldEqual` eaddr
  enterpriseAddressPaymentCred eaddr `shouldEqual` keyHashCredential pkh

pointerAddressFunctionsTest :: TestPlanM (Aff Unit) Unit
pointerAddressFunctionsTest = test "PointerAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  let
    pointer =
      { slot: wrap (BigNum.fromStringUnsafe "2147483648")
      , certIx: wrap (BigNum.fromInt 20)
      , txIx: wrap (BigNum.fromInt 120)
      }
  paddr <- doesNotThrow $ pointerAddress
    { network: MainnetId
    , paymentCred: keyHashCredential pkh
    , stakePointer: pointer
    }
  addr <- doesNotThrow $ pointerAddressToAddress paddr
  paddr2 <- errMaybe "pointerAddressFromAddress failed on valid pointer address"
    $
      pointerAddressFromAddress addr
  paddr2 `shouldEqual` paddr
  pointerAddressPaymentCred paddr `shouldEqual` keyHashCredential pkh
  pointerAddressStakePointer paddr `shouldEqual` pointer

byronAddressFunctionsTest :: TestPlanM (Aff Unit) Unit
byronAddressFunctionsTest = test "ByronAddress tests" $ log
  "ByronAddress tests todo"

suite :: TestPlanM (Aff Unit) Unit
suite = group "Address test suite" $ do
  addressFunctionsTest
  stakeCredentialTests
  baseAddressFunctionsTest
  rewardAddressFunctionsTest
  enterpriseAddressFunctionsTest
  pointerAddressFunctionsTest
  byronAddressFunctionsTest
