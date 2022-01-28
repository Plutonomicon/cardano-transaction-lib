module Serialization.Hash (
  Ed25519KeyHash,
  PubKeyHash,
  StakeKeyHash,
  ScriptHash,
  ed25519KeyHashFromBytes,
  ed25519KeyHashFromBech32,
  ed25519KeyHashToBech32,
  scriptHashToBytes,
  scriptHashToBech32Unsafe,
  scriptHashFromBytes,
  scriptHashFromBech32,
  scriptHashToBech32
  ) where

import Control.Category (identity)
import Data.Maybe (Maybe)
import FFiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization.Bech32 (Bech32String)
import Serialization.Csl (class ToCsl)
import Types.ByteArray (ByteArray)


-- | PubKeyHash and StakeKeyHash refers to blake2b-224 hash digests of Ed25519
-- | verification keys
foreign import data Ed25519KeyHash :: Type
type PubKeyHash = Ed25519KeyHash
type StakeKeyHash = Ed25519KeyHash

instance ToCsl Ed25519KeyHash Ed25519KeyHash where
  toCslRep = identity


foreign import ed25519KeyHashFromBytesImpl :: MaybeFfiHelper -> ByteArray -> Maybe Ed25519KeyHash
foreign import ed25519KeyHashFromBech32Impl :: MaybeFfiHelper -> Bech32String -> Maybe Ed25519KeyHash

foreign import ed25519KeyHashToBytes :: Ed25519KeyHash -> ByteArray
foreign import ed25519KeyHashToBech32Unsafe :: String -> Ed25519KeyHash -> Bech32String
foreign import ed25519KeyHashToBech32Impl :: MaybeFfiHelper -> String -> Ed25519KeyHash -> Maybe Bech32String

ed25519KeyHashFromBytes :: ByteArray -> Maybe Ed25519KeyHash
ed25519KeyHashFromBytes = ed25519KeyHashFromBytesImpl maybeFfiHelper

ed25519KeyHashFromBech32 :: Bech32String -> Maybe Ed25519KeyHash
ed25519KeyHashFromBech32 = ed25519KeyHashFromBech32Impl maybeFfiHelper

ed25519KeyHashToBech32 :: String -> Ed25519KeyHash -> Maybe Bech32String
ed25519KeyHashToBech32 = ed25519KeyHashToBech32Impl maybeFfiHelper

-- | blake2b-224 hash digests of serialized monetary scripts
foreign import data ScriptHash :: Type

instance ToCsl ScriptHash ScriptHash where
  toCslRep = identity

foreign import scriptHashFromBytesImpl :: MaybeFfiHelper -> ByteArray -> Maybe ScriptHash
foreign import scriptHashFromBech32Impl :: MaybeFfiHelper -> Bech32String -> Maybe ScriptHash

foreign import scriptHashToBytes :: ScriptHash -> ByteArray
foreign import scriptHashToBech32Unsafe :: String -> ScriptHash -> Bech32String
foreign import scriptHashToBech32Impl :: MaybeFfiHelper -> String -> ScriptHash -> Maybe Bech32String

scriptHashFromBytes :: ByteArray -> Maybe ScriptHash
scriptHashFromBytes = scriptHashFromBytesImpl maybeFfiHelper

scriptHashFromBech32 :: Bech32String -> Maybe ScriptHash
scriptHashFromBech32 = scriptHashFromBech32Impl maybeFfiHelper

scriptHashToBech32 :: String -> ScriptHash -> Maybe Bech32String
scriptHashToBech32 = scriptHashToBech32Impl maybeFfiHelper

