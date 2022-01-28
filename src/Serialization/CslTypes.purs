-- | Module re-exporting every CSL foreign data type
module Serialization.CslTypes(
  BaseAddressCsl,
  Ed25519KeyHash,
  ScriptHash
  ) where

import Serialization.Address as Address
import Serialization.Hash as Hash

type AddressCsl = Address.AddressCsl
type BaseAddressCsl = Address.BaseAddressCsl
type RewardAddressCsl = Address.RewardAddressCsl
type PointerAddressCsl = Address.PointerAddressCsl
type Ed25519KeyHash = Hash.Ed25519KeyHash
type ScriptHash = Hash.ScriptHash
