-- | A module for the various script types, most of which are newtype wrappers
-- | over `PlutusScript`. Corresponding hashes are also included as newtype
-- | wrappers over `ScriptHash`.
module Contract.Scripts
  ( module TypesScripts
  , module Address
  , module Scripts
  , module TypedValidator
  , module Hash
  ) where

import Address
  ( addressMintingPolicyHash
  , addressScriptHash
  , addressStakeValidatorHash
  , addressValidatorHash
  ) as Address
import Scripts
  ( mintingPolicyHash
  , scriptHash
  , stakeValidatorHash
  , typedValidatorAddress
  , typedValidatorBaseAddress
  , validatorAddress
  , validatorBaseAddress
  , validatorHash
  , validatorHashAddress
  , validatorHashBaseAddress
  ) as Scripts
import Serialization.Hash -- Includes low level helpers. Do we want these?
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
  ) as Hash
import Types.Scripts
  ( MintingPolicy(MintingPolicy)
  , MintingPolicyHash(MintingPolicyHash)
  , PlutusScript(PlutusScript)
  , StakeValidator(StakeValidator)
  , StakeValidatorHash(StakeValidatorHash)
  , Validator(Validator)
  , ValidatorHash(ValidatorHash)
  ) as TypesScripts
import Types.TypedValidator
  ( TypedValidator(TypedValidator)
  , ValidatorType
  , WrappedValidatorType
  , forwardingMintingPolicy
  , generalise
  , typedValidatorHash
  , typedValidatorScript
  ) as TypedValidator
