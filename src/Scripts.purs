module Scripts
  ( mintingPolicyHash
  , scriptHash
  , stakeValidatorHash
  , typedValidatorAddress
  , validatorAddress
  , validatorBaseAddress
  , validatorHash
  , validatorHashAddress
  , validatorHashBaseAddress
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Serialization.Address
  ( Address
  , BaseAddress
  , NetworkId
  , baseAddressToAddress
  , scriptAddress
  )
import Serialization.Hash (ScriptHash, scriptHashFromBytes)
import Types.Scripts
  ( MintingPolicy
  , MintingPolicyHash
  , PlutusScript
  , StakeValidator
  , StakeValidatorHash
  , TypedValidator(TypedValidator)
  , Validator
  , ValidatorHash
  )

-- | Helpers for `PlutusScript` and `ScriptHash` newtype wrappers, separate from
-- | the data type definitions to prevent cylic dependencies.

-- | Converts a Plutus-style `Validator` to a `BaseAddress`
validatorBaseAddress :: NetworkId -> Validator -> Maybe BaseAddress
validatorBaseAddress networkId =
  map (validatorHashBaseAddress networkId) <<< validatorHash

-- | Converts a Plutus-style `Validator` to an `Address`
validatorAddress :: NetworkId -> Validator -> Maybe Address
validatorAddress networkId =
  map baseAddressToAddress <<< validatorBaseAddress networkId

-- | Converts a Plutus-style `TypedValidator` to an `Address`
typedValidatorAddress
  :: forall (a :: Type). NetworkId -> TypedValidator a -> Address
typedValidatorAddress networkId (TypedValidator typedVal) =
  baseAddressToAddress $ scriptAddress networkId $ unwrap typedVal.validatorHash

-- | Converts a Plutus-style `MintingPolicy` to an `MintingPolicyHash`
mintingPolicyHash :: MintingPolicy -> Maybe MintingPolicyHash
mintingPolicyHash = plutusScriptHash

-- | Converts a Plutus-style `Validator` to an `ValidatorHash`
validatorHash :: Validator -> Maybe ValidatorHash
validatorHash = plutusScriptHash

-- | Converts a Plutus-style `ValidatorHash` to a `BaseAddress`
validatorHashBaseAddress :: NetworkId -> ValidatorHash -> BaseAddress
validatorHashBaseAddress networkId = scriptAddress networkId <<< unwrap

-- | Converts a Plutus-style `ValidatorHash` to an `Address`
validatorHashAddress :: NetworkId -> ValidatorHash -> Address
validatorHashAddress networkId =
  baseAddressToAddress <<< validatorHashBaseAddress networkId

-- | Converts a Plutus-style `StakeValidator` to an `Address`
stakeValidatorHash :: StakeValidator -> Maybe StakeValidatorHash
stakeValidatorHash = plutusScriptHash

plutusScriptHash
  :: forall (m :: Type) (n :: Type)
   . Newtype m PlutusScript
  => Newtype n ScriptHash
  => m
  -> Maybe n
plutusScriptHash = map wrap <<< scriptHash <<< unwrap

-- | Converts a `PlutusScript` to a `ScriptHash`.
scriptHash :: PlutusScript -> Maybe ScriptHash
scriptHash = scriptHashFromBytes <<< unwrap