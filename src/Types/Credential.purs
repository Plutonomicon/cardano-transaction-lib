module Cardano.Types.Credential where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , (.:)
  )
import Cardano.Serialization.Lib
  ( fromBytes
  , stakeCredential_fromKeyhash
  , stakeCredential_fromScripthash
  , stakeCredential_toKeyhash
  , stakeCredential_toScripthash
  , toBytes
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (class AsCbor)
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.ScriptHash (ScriptHash)
import Control.Alt ((<|>))
import Ctl.Internal.Helpers (encodeTagged')
import Ctl.Internal.Metadata.Helpers (errExpectedObject)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

-- In CSL, this type is called StakeCredential. They reuse it for Payment Credentials as well.
data Credential
  = PubKeyHashCredential Ed25519KeyHash
  | ScriptHashCredential ScriptHash

derive instance Generic Credential _
derive instance Eq Credential
derive instance Ord Credential

instance EncodeAeson Credential where
  encodeAeson = case _ of
    PubKeyHashCredential kh -> encodeTagged' "PubKeyHashCredential" kh
    ScriptHashCredential sh -> encodeTagged' "ScriptHashCredential" sh

instance DecodeAeson Credential where
  decodeAeson = caseAesonObject errExpectedObject $ \obj -> do
    tag <- obj .: "tag"
    let
      aesonContents
        :: forall (a :: Type). DecodeAeson a => Either JsonDecodeError a
      aesonContents = obj .: "content"
    case tag of
      "PubKeyHashCredential" -> PubKeyHashCredential <$> aesonContents
      "ScriptHashCredential" -> ScriptHashCredential <$> aesonContents
      _ -> Left $ TypeMismatch ("Unknown tag: " <> tag)

instance Show Credential where
  show = genericShow

instance AsCbor Credential where
  encodeCbor = toCsl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCsl

toCsl :: Credential -> Csl.StakeCredential
toCsl = case _ of
  PubKeyHashCredential kh -> stakeCredential_fromKeyhash (unwrap kh)
  ScriptHashCredential sh -> stakeCredential_fromScripthash (unwrap sh)

fromCsl :: Csl.StakeCredential -> Credential
fromCsl sc = unsafePartial $ fromJust $
  (map (PubKeyHashCredential <<< wrap) $ toMaybe $ stakeCredential_toKeyhash sc)
    <|>
      ( map (ScriptHashCredential <<< wrap) $ toMaybe $
          stakeCredential_toScripthash sc
      )
