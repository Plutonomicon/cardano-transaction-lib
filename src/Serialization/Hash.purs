module Serialization.Hash
  ( Ed25519KeyHash
  , ScriptHash
  , ed25519KeyHashToBytes
  , ed25519KeyHashFromBytes
  , ed25519KeyHashFromBech32
  , ed25519KeyHashToBech32
  , ed25519KeyHashToBech32Unsafe
  , scriptHashToBytes
  , scriptHashToBech32Unsafe
  , scriptHashFromBytes
  , scriptHashFromBech32
  , scriptHashToBech32
  ) where

import Control.Category (identity)
import Data.Eq (class Eq, eq)
import Data.Maybe (Maybe)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization.Csl (class ToCsl)
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray)

-- | PubKeyHash and StakeKeyHash refers to blake2b-224 hash digests of Ed25519
-- | verification keys
foreign import data Ed25519KeyHash :: Type

instance Eq Ed25519KeyHash where
  eq e1 e2 = eq (ed25519KeyHashToBytes e1) (ed25519KeyHashToBytes e2)

instance ToCsl Ed25519KeyHash Ed25519KeyHash where
  toCslRep = identity

foreign import _ed25519KeyHashFromBytesImpl
  :: MaybeFfiHelper
  -> ByteArray
  -> Maybe Ed25519KeyHash

foreign import _ed25519KeyHashFromBech32Impl
  :: MaybeFfiHelper
  -> Bech32String
  -> Maybe Ed25519KeyHash

foreign import ed25519KeyHashToBytes :: Ed25519KeyHash -> ByteArray

-- | Convert ed25519KeyHash to Bech32 representation with given prefix.
-- | Will crash if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
foreign import ed25519KeyHashToBech32Unsafe
  :: String
  -> Ed25519KeyHash
  -> Bech32String

foreign import _ed25519KeyHashToBech32Impl
  :: MaybeFfiHelper
  -> String
  -> Ed25519KeyHash
  -> Maybe Bech32String

ed25519KeyHashFromBytes :: ByteArray -> Maybe Ed25519KeyHash
ed25519KeyHashFromBytes = _ed25519KeyHashFromBytesImpl maybeFfiHelper

ed25519KeyHashFromBech32 :: Bech32String -> Maybe Ed25519KeyHash
ed25519KeyHashFromBech32 = _ed25519KeyHashFromBech32Impl maybeFfiHelper

-- | Convert ed25519KeyHash to Bech32 representation with given prefix.
-- | Will return Nothing if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
ed25519KeyHashToBech32 :: String -> Ed25519KeyHash -> Maybe Bech32String
ed25519KeyHashToBech32 = _ed25519KeyHashToBech32Impl maybeFfiHelper

-- | blake2b-224 hash digests of serialized monetary scripts
foreign import data ScriptHash :: Type

instance Eq ScriptHash where
  eq e1 e2 = eq (scriptHashToBytes e1) (scriptHashToBytes e2)

instance ToCsl ScriptHash ScriptHash where
  toCslRep = identity

foreign import _scriptHashFromBytesImpl
  :: MaybeFfiHelper
  -> ByteArray
  -> Maybe ScriptHash

foreign import _scriptHashFromBech32Impl
  :: MaybeFfiHelper
  -> Bech32String
  -> Maybe ScriptHash

foreign import scriptHashToBytes :: ScriptHash -> ByteArray

-- | Convert scriptHash to Bech32 representation with given prefix.
-- | Will crash if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
foreign import scriptHashToBech32Unsafe
  :: String
  -> ScriptHash
  -> Bech32String

foreign import _scriptHashToBech32Impl
  :: MaybeFfiHelper
  -> String
  -> ScriptHash
  -> Maybe Bech32String

scriptHashFromBytes :: ByteArray -> Maybe ScriptHash
scriptHashFromBytes = _scriptHashFromBytesImpl maybeFfiHelper

scriptHashFromBech32 :: Bech32String -> Maybe ScriptHash
scriptHashFromBech32 = _scriptHashFromBech32Impl maybeFfiHelper

-- | Convert scriptHash to Bech32 representation with given prefix.
-- | Will return Nothing if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
scriptHashToBech32 :: String -> ScriptHash -> Maybe Bech32String
scriptHashToBech32 = _scriptHashToBech32Impl maybeFfiHelper
