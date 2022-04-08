module Test.Serialization.Hash (suite) where

import Control.Bind (discard, bind)
import Data.Eq ((==))
import Data.Function (($))
import Data.Maybe (Maybe(..), isNothing)
import Data.Unit (Unit)
import Serialization.Hash
  ( ed25519KeyHashFromBech32
  , ed25519KeyHashFromBytes
  , ed25519KeyHashToBech32
  , ed25519KeyHashToBech32Unsafe
  , ed25519KeyHashToBytes
  , scriptHashFromBech32
  , scriptHashFromBytes
  , scriptHashToBech32
  , scriptHashToBech32Unsafe
  , scriptHashToBytes
  )
import Test.Utils (assertTrue, errMaybe)
import TestM (TestPlanM)
import Types.Aliases (Bech32String)
import Types.ByteArray (hexToByteArrayUnsafe)

pkhBech32 :: Bech32String
pkhBech32 = "addr_vkh1zuctrdcq6ctd29242w8g84nlz0q38t2lnv3zzfcrfqktx0c9tzp"

scriptHashHex :: String
scriptHashHex = "463e9264962cea64efafa576d44c8d2821d09c0c7495c253d4101a9a"

invalidBech32 :: Bech32String
invalidBech32 = "addr_vkh1zuctrdcq6ctd29242w8g8444z0q38t2lnv3zzf44fqktx044444"

suite :: TestPlanM Unit
suite = do
  assertTrue "ed25519KeyHashFromBech32 returns Nothing on random string"
    (isNothing $ ed25519KeyHashFromBech32 invalidBech32)

  pkh <- errMaybe "ed25519KeyHashFromBech32 failed" $ ed25519KeyHashFromBech32
    pkhBech32
  let
    pkhB32 = ed25519KeyHashToBech32Unsafe "addr_vkh" pkh
    mPkhB32 = ed25519KeyHashToBech32 "addr_vkh" pkh
    pkhBts = ed25519KeyHashToBytes pkh
    pkh2 = ed25519KeyHashFromBytes pkhBts

  assertTrue
    "Safe ed25519KeyHashToBech32 should produce Just when unsafe version works"
    (mPkhB32 == Just pkhB32)

  assertTrue
    "Safe ed25519KeyHashToBech32 should return Nothing on invalid prefix"
    (ed25519KeyHashToBech32 "" pkh == Nothing)

  assertTrue "ed25519KeyHashFromBytes does not reverts ed25519KeyHashToBytes"
    (pkh2 == Just pkh)

  assertTrue
    "ed25519KeyHashFromBech32 does not reverts ed25519KeyHashToBech32Unsafe"
    (pkhB32 == pkhBech32)

  --

  assertTrue "scriptHashFromBech32 returns Nothing on random string"
    (isNothing $ scriptHashFromBech32 invalidBech32)

  scrh <- errMaybe "scriptHashFromBytes failed" $ scriptHashFromBytes $
    hexToByteArrayUnsafe
      scriptHashHex
  let
    scrhB32 = scriptHashToBech32Unsafe "stake_vkh" scrh
    mScrhB32 = scriptHashToBech32 "stake_vkh" scrh
    scrhBts = scriptHashToBytes scrh
    scrhFromBytes = scriptHashFromBytes scrhBts
    scrhFromBech = scriptHashFromBech32 scrhB32

  assertTrue "Safe scriptHashToBech32 should produce Just when unsafe works"
    (mScrhB32 == Just scrhB32)

  assertTrue "Safe scriptHashToBech32 should return Nothing on invalid prefix"
    (scriptHashToBech32 "" scrh == Nothing)

  assertTrue "scriptHashFromBytes does not reverts scriptHashToBytes"
    (scrhFromBytes == Just scrh)

  assertTrue "scriptHashFromBech32 does not reverts scriptHashToBech32Unsafe"
    (scrhFromBech == Just scrh)
