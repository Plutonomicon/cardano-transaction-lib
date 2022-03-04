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
import Deserialization.WitnessSet (plutusScriptBytes)
import Effect (Effect)
import Serialization.Address
  ( Address
  , BaseAddress
  , NetworkId
  , baseAddressToAddress
  , scriptAddress
  )
import Serialization.Hash (ScriptHash, scriptHashFromBytes)
import Serialization.WitnessSet (convertPlutusScript)
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

-- Helpers for `PlutusScript` and `ScriptHash` newtype wrappers, have it separate
-- to prevent cylic dependencies.

-- | Converts a Plutus-style `Validator` to a `BaseAddress`
validatorBaseAddress :: NetworkId -> Validator -> Effect (Maybe BaseAddress)
validatorBaseAddress networkId =
  map (map (validatorHashBaseAddress networkId)) <<< validatorHash

-- | Converts a Plutus-style `Validator` to an `Address`
validatorAddress :: NetworkId -> Validator -> Effect (Maybe Address)
validatorAddress networkId =
  map (map baseAddressToAddress) <<< validatorBaseAddress networkId

-- | Converts a Plutus-style `TypedValidator` to an `Address`
typedValidatorAddress
  :: forall (a :: Type). NetworkId -> TypedValidator a -> Address
typedValidatorAddress networkId (TypedValidator typedVal) =
  baseAddressToAddress $ scriptAddress networkId $ unwrap typedVal.validatorHash

-- | Converts a Plutus-style `MintingPolicy` to an `MintingPolicyHash`
mintingPolicyHash :: MintingPolicy -> Effect (Maybe MintingPolicyHash)
mintingPolicyHash = plutusScriptHash

-- | Converts a Plutus-style `Validator` to an `ValidatorHash`
validatorHash :: Validator -> Effect (Maybe ValidatorHash)
validatorHash = plutusScriptHash

-- | Converts a Plutus-style `ValidatorHash` to a `BaseAddress`
validatorHashBaseAddress :: NetworkId -> ValidatorHash -> BaseAddress
validatorHashBaseAddress networkId = scriptAddress networkId <<< unwrap

-- | Converts a Plutus-style `ValidatorHash` to an `Address`
validatorHashAddress :: NetworkId -> ValidatorHash -> Address
validatorHashAddress networkId =
  baseAddressToAddress <<< validatorHashBaseAddress networkId

-- | Converts a Plutus-style `StakeValidator` to an `Address`
stakeValidatorHash :: StakeValidator -> Effect (Maybe StakeValidatorHash)
stakeValidatorHash = plutusScriptHash

plutusScriptHash
  :: forall (m :: Type) (n :: Type)
   . Newtype m PlutusScript
  => Newtype n ScriptHash
  => m
  -> Effect (Maybe n)
plutusScriptHash = map (map wrap) <<< scriptHash <<< unwrap

-- | Converts a `PlutusScript` to a `ScriptHash`.
scriptHash :: PlutusScript -> Effect (Maybe ScriptHash)
scriptHash =
  map (scriptHashFromBytes <<< plutusScriptBytes)
    <<< convertPlutusScript