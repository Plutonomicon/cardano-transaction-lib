module Test.Serialization.Address (suite) where

import Prelude

import Data.Maybe (Maybe, isNothing)
import Data.Newtype (wrap)
import Data.Typelevel.Undefined (undefined)
import Data.UInt (fromInt)
import Mote (group)
import Serialization.Address (addressBech32, addressBytes, addressFromBech32, addressFromBytes, addressNetworkId, baseAddressDelegationCred, baseAddressFromAddress, baseAddressPaymentCred, baseAddressToAddress, enterpriseAddress, enterpriseAddressFromAddress, enterpriseAddressPaymentCred, enterpriseAddressToAddress, keyHashCredential, mainnetId, pointerAddress, pointerAddressFromAddress, pointerAddressPaymentCred, pointerAddressStakePointer, pointerAddressToAddress, pubKeyAddress, rewardAddress, rewardAddressFromAddress, rewardAddressPaymentCred, rewardAddressToAddress, scriptHashCredential, stakeCredentialFromBytes, stakeCredentialToBytes, stakeCredentialToKeyHash, stakeCredentialToScriptHash, testnetId)
import Serialization.Hash (Ed25519KeyHash, ScriptHash, ed25519KeyHashFromBech32, scriptHashFromBytes)
import Test.Utils (assertTrue_, errMaybe)
import TestM (TestPlanM)
import Types.Aliases (Bech32String)
import Types.ByteArray (hexToByteArrayUnsafe)

doesNotThrow :: forall a. a -> TestPlanM a
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
addressFunctionsTest = group "Address tests" $ do
  let bechstr = "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"
  addr1 <- errMaybe "addressFromBech32 failed on valid bech32" $ addressFromBech32 bechstr
  assertTrue_ $ bechstr == addressBech32 addr1
  assertTrue_ $ isNothing $ addressFromBech32 "randomstuff"
  let addrBts = addressBytes addr1
  addr2 <- errMaybe "addressFromBech32 failed on valid bech32" $ addressFromBytes addrBts
  assertTrue_ $ addr2 == addr1
  assertTrue_ $ addressNetworkId addr2 == mainnetId

stakeCredentialTests :: TestPlanM Unit
stakeCredentialTests = group "StakeCredential tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  scrh <- errMaybe "Error scriptHashFromBech32:" mScriptHash

  let
    pkhCred = keyHashCredential $ pkh
    schCred = scriptHashCredential $ scrh
    pkhCredBytes = stakeCredentialToBytes pkhCred
    schCredBytes = stakeCredentialToBytes schCred

  pkhCred2 <- errMaybe "stakeCredentialFromBytes failed on valid bytes" $ stakeCredentialFromBytes pkhCredBytes
  pkh2 <- errMaybe "stakeCredentialToKeyHash failed" $ stakeCredentialToKeyHash pkhCred2
  assertTrue_ $ pkh2 == pkh
  assertTrue_ $ isNothing $ stakeCredentialToScriptHash pkhCred2

  schCred2 <- errMaybe "stakeCredentialFromBytes failed on valid bytes" $ stakeCredentialFromBytes schCredBytes
  sch2 <- errMaybe "stakeCredentialToScriptHash failed" $ stakeCredentialToScriptHash schCred2
  assertTrue_ $ sch2 == scrh
  assertTrue_ $ isNothing $ stakeCredentialToKeyHash schCred2

baseAddressFunctionsTest :: TestPlanM Unit
baseAddressFunctionsTest = group "BaseAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  baddr <- doesNotThrow $ pubKeyAddress mainnetId pkh
  addr <- doesNotThrow $ baseAddressToAddress baddr
  baddr2 <- errMaybe "baseAddressFromAddress failed on valid base address" $ baseAddressFromAddress addr
  assertTrue_ $ baddr2 == baddr
  assertTrue_ $ baseAddressDelegationCred baddr == keyHashCredential pkh
  assertTrue_ $ baseAddressPaymentCred baddr == keyHashCredential pkh

rewardAddressFunctionsTest :: TestPlanM Unit
rewardAddressFunctionsTest =
  group "RewardAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  raddr <- doesNotThrow $ rewardAddress { network: testnetId, paymentCred: keyHashCredential pkh }
  addr <- doesNotThrow $ rewardAddressToAddress raddr
  raddr2 <- errMaybe "rewardAddressFromAddress failed on valid reward address" $ rewardAddressFromAddress addr
  assertTrue_ $ raddr2 == raddr
  assertTrue_ $ rewardAddressPaymentCred raddr == keyHashCredential pkh

enterpriseAddressFunctionsTest :: TestPlanM Unit
enterpriseAddressFunctionsTest = group "EnterpriseAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  eaddr <- doesNotThrow $ enterpriseAddress { network: mainnetId, paymentCred: keyHashCredential pkh }
  addr <- doesNotThrow $ enterpriseAddressToAddress eaddr
  eaddr2 <- errMaybe "enterpriseAddressFromAddress failed on valid enterprise address" $ enterpriseAddressFromAddress addr
  assertTrue_ $ eaddr2 == eaddr
  assertTrue_ $ enterpriseAddressPaymentCred eaddr == keyHashCredential pkh

pointerAddressFunctionsTest :: TestPlanM Unit
pointerAddressFunctionsTest = group "PointerAddress tests" $ do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  let pointer = { slot: wrap (fromInt (-2147483648)), certIx: wrap (fromInt 20), txIx: wrap (fromInt 120) }
  paddr <- doesNotThrow $ pointerAddress { network: mainnetId, paymentCred: keyHashCredential pkh, stakePointer: pointer }
  addr <- doesNotThrow $ pointerAddressToAddress paddr
  paddr2 <- errMaybe "pointerAddressFromAddress failed on valid pointer address" $ pointerAddressFromAddress addr
  assertTrue_ $ paddr2 == paddr
  assertTrue_ $ pointerAddressPaymentCred paddr == keyHashCredential pkh

  assertTrue_ $ pointerAddressStakePointer paddr == pointer

byronAddressFunctionsTest :: TestPlanM Unit
byronAddressFunctionsTest = group "ByronAddress tests" $ undefined

suite :: TestPlanM Unit
suite = do
  addressFunctionsTest
  stakeCredentialTests
  baseAddressFunctionsTest
  rewardAddressFunctionsTest
  enterpriseAddressFunctionsTest
  pointerAddressFunctionsTest
