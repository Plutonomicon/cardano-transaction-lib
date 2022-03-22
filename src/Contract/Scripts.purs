-- | A module for various script types, most of which are newtype wrappers
-- | over `PlutusScript`. Corresponding hashes are also included as newtype
-- | wrappers over `ScriptHash`.
module Contract.Scripts
  ( applyArgs
  , applyArgsM
  , mintingPolicyHash
  , stakeValidatorHash
  , validatorAddress
  , validatorBaseAddress
  , validatorHash
  , module Address
  , module ExportQueryM
  , module ExportScripts
  , module Hash
  , module TypedValidator
  , module TypesScripts
  ) where

import Address
  ( addressMintingPolicyHash
  , addressScriptHash
  , addressStakeValidatorHash
  , addressValidatorHash
  ) as Address
import QueryM
  ( ClientError
      ( ClientHttpError
      , ClientDecodeJsonError
      , ClientEncodingError
      )
  ) as ExportQueryM
import QueryM (applyArgs) as QueryM
import Scripts
  ( typedValidatorAddress
  , typedValidatorBaseAddress
  , validatorHashAddress
  , validatorHashBaseAddress
  , scriptHash
  ) as ExportScripts
import Scripts
  ( mintingPolicyHash
  , stakeValidatorHash
  , validatorAddress
  , validatorBaseAddress
  , validatorHash
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
  , class DatumType
  , class RedeemerType
  , class ValidatorTypes
  , forwardingMintingPolicy
  , generalise
  , typedValidatorHash
  , typedValidatorScript
  ) as TypedValidator

import Prelude
import Contract.Monad (Contract)
import Data.Argonaut (class DecodeJson)
import Data.Either (Either, hush)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap)
import Serialization.Address (Address, BaseAddress)
import Types.PlutusData (PlutusData)
import Types.Scripts
  ( MintingPolicy
  , MintingPolicyHash
  , PlutusScript
  , StakeValidator
  , StakeValidatorHash
  , Validator
  , ValidatorHash
  )

-- | Apply `PlutusData` arguments to any type isomorphic to `PlutusScript`,
-- | returning an updated script with the provided arguments applied
applyArgs
  :: forall (a :: Type)
   . Newtype a PlutusScript
  => DecodeJson a
  => a
  -> Array PlutusData
  -> Contract (Either ExportQueryM.ClientError a)
applyArgs a = wrap <<< QueryM.applyArgs a

-- | Same as `applyArgs` with arguments hushed.
applyArgsM
  :: forall (a :: Type)
   . Newtype a PlutusScript
  => DecodeJson a
  => a
  -> Array PlutusData
  -> Contract (Maybe a)
applyArgsM a = map hush <<< applyArgs a

mintingPolicyHash :: MintingPolicy -> Contract (Maybe MintingPolicyHash)
mintingPolicyHash = wrap <<< Scripts.mintingPolicyHash

stakeValidatorHash :: StakeValidator -> Contract (Maybe StakeValidatorHash)
stakeValidatorHash = wrap <<< Scripts.stakeValidatorHash

validatorHash :: Validator -> Contract (Maybe ValidatorHash)
validatorHash = wrap <<< Scripts.validatorHash

validatorAddress :: Validator -> Contract (Maybe Address)
validatorAddress = wrap <<< Scripts.validatorAddress

validatorBaseAddress :: Validator -> Contract (Maybe BaseAddress)
validatorBaseAddress = wrap <<< Scripts.validatorBaseAddress
