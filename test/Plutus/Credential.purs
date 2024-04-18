module Test.Ctl.Internal.Plutus.Credential
  ( suite
  , creds
  ) where

import Prelude

import Cardano.Types (Bech32String)
import Cardano.Types.Credential
  ( Credential(ScriptHashCredential, PubKeyHashCredential)
  )
import Cardano.Types.Ed25519KeyHash as Ed25519KeyHash
import Cardano.Types.ScriptHash as ScriptHash
import Mote.TestPlanM (TestPlanM)
import Data.Maybe (fromJust)
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Mote (group)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Utils (toFromAesonTest)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Plutus.Types.Credential" $ do
    group "Aeson tests" $ do
      group "Roundtrip tests" $ for_ creds $ toFromAesonTest "Credential"

creds :: Array Credential
creds =
  [ pubKeyCredential
  , scriptCredential
  ]

paymentKeyBech32 :: Bech32String
paymentKeyBech32 =
  "addr_vkh1jjfnzhxe966a33psfenm0ct2udkkr569qf55v4uprgkgu8zsvmg"

pubKeyCredential :: Credential
pubKeyCredential =
  PubKeyHashCredential <<< unsafePartial fromJust $
    Ed25519KeyHash.fromBech32 paymentKeyBech32

scriptBech32 :: Bech32String
scriptBech32 =
  "script1cda3khwqv60360rp5m7akt50m6ttapacs8rqhn5w342z7r35m37"

scriptCredential :: Credential
scriptCredential =
  ScriptHashCredential <<< unsafePartial fromJust $
    ScriptHash.fromBech32 scriptBech32
