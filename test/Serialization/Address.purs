module Test.Serialization.Address (suite) where

import Prelude

import Data.Maybe (Maybe, isNothing)
import Serialization.Address
  ( NetworkTag(..)
  , addressBech32
  , addressBytes
  , baseAddress
  , baseAddressFromBech32
  , baseAddressFromBytes
  , delegationCred
  , ed25519KeyHashCredType
  , paymentCred
  , pubKeyAddress
  , rewardAddress
  , rewardAddressFromBech32
  , rewardAddressFromBytes
  , scriptAddress
  , scriptHashCredType
  , toAddressCsl
  )
import Serialization.Hash
  ( Ed25519KeyHash
  , ScriptHash
  , ed25519KeyHashFromBech32
  , scriptHashFromBytes
  )
import Test.Utils (assertTrue, assertTrue_, errMaybe, unsafeCall)
import TestM (TestPlanM)
import Type.Prelude (Proxy(..))
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray, hexToByteArrayUnsafe)

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

suite :: TestPlanM Unit
suite = do
  pkh <- errMaybe "Error ed25519KeyHashFromBech32:" mPkh
  scrh <- errMaybe "Error scriptHashFromBech32:" mScriptHash

  -- address constructors
  addr1 <- doesNotThrow $ pubKeyAddress MainnetTag pkh
  addr2 <- doesNotThrow $ baseAddress { network: MainnetTag, payment: pkh, delegation: pkh }
  raddr1 <- doesNotThrow $ rewardAddress { network: MainnetTag, payment: pkh }
  assertTrue_ (addr1 == addr2)

  assertTrue_ $ paymentCred raddr1 == paymentCred addr1

  saddr1 <- doesNotThrow $ scriptAddress TestnetTag scrh
  saddr2 <- doesNotThrow $ baseAddress { network: TestnetTag, payment: scrh, delegation: scrh }
  assertTrue_ (saddr1 == saddr2)

  -- preserves pkh and scripthash
  let
    pkh1 = paymentCred addr1
    pkh2 = delegationCred addr1
  assertTrue_ (pkh1 == pkh)
  assertTrue_ (pkh2 == pkh)

  let
    scrh1 = paymentCred saddr1
    scrh2 = delegationCred saddr1
  assertTrue_ (scrh1 == scrh)
  assertTrue_ (scrh2 == scrh)

  -- address bech32
  let ab32 = addressBech32 addr1
  addr3 <- errMaybe "baseAddressFromBech32 fails to decode valid bech32" $
    baseAddressFromBech32 { payment: ed25519KeyHashCredType, delegation: ed25519KeyHashCredType }
      ab32
  assertTrue_ (addr1 == addr3)

  let sab32 = addressBech32 saddr1
  saddr3 <- errMaybe "baseAddressFromBech32 fails to decode valid bech32" $
    baseAddressFromBech32 { payment: scriptHashCredType, delegation: scriptHashCredType }
      sab32

  assertTrue_ (saddr1 == saddr3)

  let rb32 = addressBech32 raddr1
  raddr3 <- errMaybe "baseAddressFromBech32 fails to decode valid bech32" $
    rewardAddressFromBech32 { payment: ed25519KeyHashCredType }
      rb32

  assertTrue_ (raddr1 == raddr3)

  assertTrue "rewardAddressFromBech32 mistakes redential type" $ isNothing $
    rewardAddressFromBech32 { payment: scriptHashCredType }
      rb32
  assertTrue "baseAddressFromBech32 mistakes address payment credential type" $ isNothing $
    baseAddressFromBech32 { payment: scriptHashCredType, delegation: ed25519KeyHashCredType }
      ab32
  assertTrue "baseAddressFromBech32 mistakes address payment credential type" $ isNothing $
    baseAddressFromBech32 { payment: ed25519KeyHashCredType, delegation: scriptHashCredType }
      sab32
  assertTrue "baseAddressFromBech32 mistakes address delegation credential type" $ isNothing $
    baseAddressFromBech32 { payment: ed25519KeyHashCredType, delegation: scriptHashCredType }
      ab32
  assertTrue "baseAddressFromBech32 mistakes address delegation credential type" $ isNothing $
    baseAddressFromBech32 { payment: scriptHashCredType, delegation: ed25519KeyHashCredType }
      sab32

  -- address bytes
  let
    abts = addressBytes addr1
    sabts = addressBytes saddr1
    rabts = addressBytes raddr1
  addr5 <- errMaybe "baseAddressFromBytes fails to decode valid bech32" $
    baseAddressFromBytes { payment: ed25519KeyHashCredType, delegation: ed25519KeyHashCredType }
      abts

  assertTrue_ (addr1 == addr5)

  saddr5 <- errMaybe "baseAddressFromBytes fails to decode valid bech32" $
    baseAddressFromBytes { payment: scriptHashCredType, delegation: scriptHashCredType }
      sabts

  assertTrue_ (saddr1 == saddr5)

  assertTrue "rewardAddressFromBytes mistakes redential type" $ isNothing $
    rewardAddressFromBytes { payment: scriptHashCredType }
      rabts

  assertTrue "baseAddressFromBech32 mistakes address payment credential type" $ isNothing $
    baseAddressFromBytes { payment: scriptHashCredType, delegation: ed25519KeyHashCredType }
      abts

  assertTrue "baseAddressFromBech32 mistakes address payment credential type" $ isNothing $
    baseAddressFromBytes { payment: ed25519KeyHashCredType, delegation: scriptHashCredType }
      sabts

  assertTrue "baseAddressFromBytes mistakes address delegation credential type" $ isNothing $
    baseAddressFromBytes { payment: ed25519KeyHashCredType, delegation: scriptHashCredType }
      abts

  assertTrue "baseAddressFromBytes mistakes address delegation credential type" $ isNothing $
    baseAddressFromBytes { payment: scriptHashCredType, delegation: ed25519KeyHashCredType }
      sabts

  -- addresscsl
  let
    frn = toAddressCsl addr1
    frn2 = toAddressCsl raddr1
    frnBts = unsafeCall (Proxy :: _ ByteArray) "to_bytes" frn
    frnBts2 = unsafeCall (Proxy :: _ ByteArray) "to_bytes" frn2

  assertTrue_ (frnBts == abts)
  assertTrue_ (frnBts2 == rabts)
  assertTrue_ (addr1 == addr2)
