module Ctl.Internal.Scripts
  ( validatorHash
  ) where

import Prelude

import Ctl.Internal.Hashing (mintingPolicyHash, plutusScriptHash)
import Ctl.Internal.NativeScripts (NativeScriptHash, nativeScriptHash)
import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol(CurrencySymbol))
import Ctl.Internal.Serialization.Hash (scriptHashToBytes)
import Ctl.Internal.Types.Scripts
  ( MintingPolicy(NativeMintingPolicy, PlutusMintingPolicy)
  , MintingPolicyHash
  , NativeScriptStakeValidator
  , PlutusScriptStakeValidator
  , StakeValidatorHash
  , Validator
  , ValidatorHash
  )
import Data.Newtype (unwrap, wrap)

-- | Helpers for `PlutusScript` and `ScriptHash` newtype wrappers, separate from
-- | the data type definitions to prevent cylic dependencies.

-- | Converts a Plutus-style `Validator` to an `ValidatorHash`
validatorHash :: Validator -> ValidatorHash
validatorHash = wrap <<< plutusScriptHash <<< unwrap

-- | Calculates a hash of a NativeScript stake validator
nativeScriptStakeValidatorHash
  :: NativeScriptStakeValidator -> StakeValidatorHash
nativeScriptStakeValidatorHash = unwrap >>> nativeScriptHash >>> unwrap >>> wrap
