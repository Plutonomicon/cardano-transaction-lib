module Test.Ctl.Plutus.Credential
  ( suite
  , creds
  ) where

import Prelude

import Ctl.Internal.Serialization.Hash
  ( ed25519KeyHashFromBech32
  , scriptHashFromBech32
  )
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Plutus.Types.Credential
  ( Credential(ScriptCredential, PubKeyCredential)
  )
import Data.Maybe (fromJust)
import Data.Newtype (wrap)
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Mote (group)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.TestM (TestPlanM)
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
  PubKeyCredential <<< wrap <<< unsafePartial fromJust $
    ed25519KeyHashFromBech32 paymentKeyBech32

scriptBech32 :: Bech32String
scriptBech32 =
  "script1cda3khwqv60360rp5m7akt50m6ttapacs8rqhn5w342z7r35m37"

scriptCredential :: Credential
scriptCredential =
  ScriptCredential <<< wrap <<< unsafePartial fromJust $
    scriptHashFromBech32 scriptBech32
