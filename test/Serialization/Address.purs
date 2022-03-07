module Test.Serialization.Address (suite) where

import Prelude

import Data.BigInt (fromInt) as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (wrap)
import Data.UInt (fromInt) as UInt
import Effect.Class.Console (log)
import Mote (group, test)
import Serialization.Address
  ( addressBech32
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
  , mainnetId
  , pointerAddress
  , pointerAddressFromAddress
  , pointerAddressPaymentCred
  , pointerAddressStakePointer
  , pointerAddressToAddress
  , pubKeyAddress
  , rewardAddress
  , rewardAddressFromAddress
  , rewardAddressPaymentCred
  , rewardAddressToAddress
  , scriptHashCredential
  , stakeCredentialFromBytes
  , stakeCredentialToBytes
  , stakeCredentialToKeyHash
  , stakeCredentialToScriptHash
  , testnetId
  )
import Serialization.Hash (Ed25519KeyHash, ScriptHash, ed25519KeyHashFromBech32, scriptHashFromBytes)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (errMaybe)
import TestM (TestPlanM)
import Types.Aliases (Bech32String)
import Types.ByteArray (hexToByteArrayUnsafe)

doesNotThrow :: forall (f :: Type -> Type) (a :: Type). Applicative f => a -> f a
doesNotThrow = pure

pkhBech32 :: Bech32String
pkhBech32 = "addr_vkh1zuctrdcq6ctd29242w8g84nlz0q38t2lnv3zzfcrfqktx0c9tzp"

scriptHashHex :: String
scriptHashHex = "463e9264962cea64efafa576d44c8d2821d09c0c7495c253d4101a9a"

mPkh :: Maybe Ed25519KeyHash
mPkh = ed25519KeyHashFromBech32 pkhBech32

mScriptHash :: Maybe ScriptHash
mScriptHash = scriptHashFromBytes $ hexToByteArrayUnsafe scriptHashHex

addressFunctionsTest :: TestPlanM Unit
addressFunctionsTest = test "Address tests" $ do
  let bechstr = "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"
  addr1 <- errMaybe "addressFromBech32 failed on valid bech32" $ addressFromBech32 bechstr
  bechstr `shouldEqual` addressBech32 addr1
  addressFromBech32 "randomstuff" `shouldEqual` Nothing
  let addrBts = addressBytes addr1
  addr2 <- errMaybe "addressFromBech32 failed on valid bech32" $ addressFromBytes addrBts
  addr2 `shouldEqual` addr1
  addressNetworkId addr2 `shouldEqual` mainnetId

stakeCredentialTests :: TestPlanM Unit
stakeCredentialTests = test "StakeCredential tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  scrh <- errMaybe "Error scriptHashFromBech32:" mScriptHash

  let
    pkhCred = keyHashCredential $ pkh
    schCred = scriptHashCredential $ scrh
    pkhCredBytes = stakeCredentialToBytes pkhCred
    schCredBytes = stakeCredentialToBytes schCred

  pkhCred2 <- errMaybe "stakeCredentialFromBytes failed on valid bytes" $ stakeCredentialFromBytes pkhCredBytes
  pkh2 <- errMaybe "stakeCredentialToKeyHash failed" $ stakeCredentialToKeyHash pkhCred2
  pkh2 `shouldEqual` pkh
  stakeCredentialToScriptHash pkhCred2 `shouldEqual` Nothing

  schCred2 <- errMaybe "takeCredentialFromBytes failed on valid bytes" $ stakeCredentialFromBytes schCredBytes
  sch2 <- errMaybe "stakeCredentialToScriptHash failed" $ stakeCredentialToScriptHash schCred2
  sch2 `shouldEqual` scrh
  stakeCredentialToKeyHash schCred2 `shouldEqual` Nothing

baseAddressFunctionsTest :: TestPlanM Unit
baseAddressFunctionsTest = test "BaseAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  baddr <- doesNotThrow $ pubKeyAddress mainnetId pkh
  addr <- doesNotThrow $ baseAddressToAddress baddr
  baddr2 <- errMaybe "baseAddressFromAddress failed on valid base address" $ baseAddressFromAddress addr
  baddr2 `shouldEqual` baddr
  baseAddressDelegationCred baddr `shouldEqual` keyHashCredential pkh
  baseAddressPaymentCred baddr `shouldEqual` keyHashCredential pkh

rewardAddressFunctionsTest :: TestPlanM Unit
rewardAddressFunctionsTest = test "RewardAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  raddr <- doesNotThrow $ rewardAddress { network: testnetId, paymentCred: keyHashCredential pkh }
  addr <- doesNotThrow $ rewardAddressToAddress raddr
  raddr2 <- errMaybe "rewardAddressFromAddress failed on valid reward address" $ rewardAddressFromAddress addr
  raddr2 `shouldEqual` raddr
  rewardAddressPaymentCred raddr `shouldEqual` keyHashCredential pkh

enterpriseAddressFunctionsTest :: TestPlanM Unit
enterpriseAddressFunctionsTest = test "EnterpriseAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  eaddr <- doesNotThrow $ enterpriseAddress { network: mainnetId, paymentCred: keyHashCredential pkh }
  addr <- doesNotThrow $ enterpriseAddressToAddress eaddr
  eaddr2 <- errMaybe "enterpriseAddressFromAddress failed on valid enterprise address" $ enterpriseAddressFromAddress addr
  eaddr2 `shouldEqual` eaddr
  enterpriseAddressPaymentCred eaddr `shouldEqual` keyHashCredential pkh

pointerAddressFunctionsTest :: TestPlanM Unit
pointerAddressFunctionsTest = test "PointerAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  let pointer = { slot: wrap (BigInt.fromInt (-2147483648)), certIx: wrap (UInt.fromInt 20), txIx: wrap (UInt.fromInt 120) }
  paddr <- doesNotThrow $ pointerAddress { network: mainnetId, paymentCred: keyHashCredential pkh, stakePointer: pointer }
  addr <- doesNotThrow $ pointerAddressToAddress paddr
  paddr2 <- errMaybe "pointerAddressFromAddress failed on valid pointer address" $ pointerAddressFromAddress addr
  paddr2 `shouldEqual` paddr
  pointerAddressPaymentCred paddr `shouldEqual` keyHashCredential pkh
  pointerAddressStakePointer paddr `shouldEqual` pointer

byronAddressFunctionsTest :: TestPlanM Unit
byronAddressFunctionsTest = test "ByronAddress tests" $ log "ByronAddress tests todo"

suite :: TestPlanM Unit
suite = group "Address test suite" $ do
  addressFunctionsTest
  stakeCredentialTests
  baseAddressFunctionsTest
  rewardAddressFunctionsTest
  enterpriseAddressFunctionsTest
  pointerAddressFunctionsTest
  byronAddressFunctionsTest
