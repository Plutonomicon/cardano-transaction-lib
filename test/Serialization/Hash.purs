module Test.Ctl.Serialization.Hash (suite) where

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Types.Ed25519KeyHash as Ed25519KeyHash
import Cardano.Types.ScriptHash as ScriptHash
import Control.Bind (bind, discard)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.Aliases (Bech32String)
import Data.ByteArray (hexToByteArrayUnsafe)
import Data.Eq ((==))
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Newtype (wrap)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Mote (test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Utils (assertTrue, errMaybe)

pkhBech32 :: Bech32String
pkhBech32 = "addr_vkh1zuctrdcq6ctd29242w8g84nlz0q38t2lnv3zzfcrfqktx0c9tzp"

scriptHashHex :: String
scriptHashHex = "463e9264962cea64efafa576d44c8d2821d09c0c7495c253d4101a9a"

invalidBech32 :: Bech32String
invalidBech32 = "addr_vkh1zuctrdcq6ctd29242w8g8444z0q38t2lnv3zzf44fqktx044444"

suite :: TestPlanM (Aff Unit) Unit
suite = unsafePartial $ test "Serialization.Hash" do
  assertTrue "ed25519KeyHashFromBech32 returns Nothing on random string"
    (isNothing $ Ed25519KeyHash.fromBech32 invalidBech32)

  pkh <- errMaybe "ed25519KeyHashFromBech32 failed" $ Ed25519KeyHash.fromBech32
    pkhBech32
  let
    pkhB32 = Ed25519KeyHash.toBech32Unsafe "addr_vkh" pkh
    mPkhB32 = Ed25519KeyHash.toBech32 "addr_vkh" pkh
    pkhBts = encodeCbor pkh
    pkh2 = decodeCbor pkhBts

  assertTrue
    "Safe Ed25519KeyHash.toBech32 should produce Just when unsafe version works"
    (mPkhB32 == Just pkhB32)

  assertTrue
    "Safe Ed25519KeyHash.toBech32 should return Nothing on invalid prefix"
    (Ed25519KeyHash.toBech32 "" pkh == Nothing)

  assertTrue "ed25519KeyHashFromBytes does not reverts ed25519KeyHashToBytes"
    (pkh2 == Just pkh)

  assertTrue
    "ed25519KeyHashFromBech32 does not reverts Ed25519KeyHash.toBech32Unsafe"
    (pkhB32 == pkhBech32)

  --

  assertTrue "scriptHashFromBech32 returns Nothing on random string"
    (isNothing $ ScriptHash.fromBech32 invalidBech32)

  scrh <- errMaybe "scriptHashFromBytes failed" $ decodeCbor
    $ wrap $ hexToByteArrayUnsafe scriptHashHex
  let
    scrhB32 = ScriptHash.toBech32Unsafe "stake_vkh" scrh
    mScrhB32 = ScriptHash.toBech32Unsafe "stake_vkh" scrh
    scrhBts = encodeCbor scrh
    scrhFromBech = ScriptHash.fromBech32 scrhB32

  assertTrue "ScriptHash.fromBech32 does not reverts ScriptHash.toBech32Unsafe"
    (scrhFromBech == Just scrh)
