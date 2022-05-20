-- | A module for various script types, most of which are newtype wrappers
-- | over `PlutusScript`. Corresponding hashes are also included as newtype
-- | wrappers over `ScriptHash`.
module Contract.Scripts
  ( applyArgs
  , applyArgsM
  , mintingPolicyHash
  , stakeValidatorHash
  , validatorHash
  , module Address
  , module ExportQueryM
  , module ExportScripts
  , module Hash
  , module TypedValidator
  , module TypesScripts
  ) where

import Aeson (class DecodeAeson)
import Address
  ( enterpriseAddressMintingPolicyHash
  , enterpriseAddressScriptHash
  )
-- See Contract.Address for documentation on the various helpers, some are
-- constructive/deconstructive on the Plutus `Address` type, others are from
-- the CSL API and converted to use Plutus types.
import Contract.Address
  ( enterpriseAddressScriptHash
  , enterpriseAddressStakeValidatorHash
  , enterpriseAddressValidatorHash
  , scriptHashAddress -- Directly uses Plutus `Address`
  , toValidatorHash -- Directly uses Plutus `Address`
  , typedValidatorBaseAddress
  , typedValidatorEnterpriseAddress
  , validatorHashBaseAddress
  , validatorHashEnterpriseAddress
  ) as Address
import QueryM
  ( ClientError
      ( ClientHttpError
      , ClientDecodeJsonError
      , ClientEncodingError
      )
  ) as ExportQueryM
import QueryM (applyArgs) as QueryM
import Scripts (scriptHash) as ExportScripts
import Scripts
  ( mintingPolicyHash
  , stakeValidatorHash
  , validatorHash
  ) as Scripts
import Serialization.Hash (ScriptHash) as Hash
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
import Contract.Monad (Contract, wrapContract)
import Data.Either (Either, hush)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
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
  :: forall (r :: Row Type) (a :: Type)
   . Newtype a PlutusScript
  => DecodeAeson a
  => a
  -> Array PlutusData
  -> Contract r (Either ExportQueryM.ClientError a)
applyArgs a = wrapContract <<< QueryM.applyArgs a

-- | Same as `applyArgs` with arguments hushed.
applyArgsM
  :: forall (r :: Row Type) (a :: Type)
   . Newtype a PlutusScript
  => DecodeAeson a
  => a
  -> Array PlutusData
  -> Contract r (Maybe a)
applyArgsM a = map hush <<< applyArgs a

-- | Converts a Plutus-style `MintingPolicy` to an `MintingPolicyHash`
mintingPolicyHash
  :: forall (r :: Row Type)
   . MintingPolicy
  -> Contract r (Maybe MintingPolicyHash)
mintingPolicyHash = wrapContract <<< Scripts.mintingPolicyHash

-- | Converts a Plutus-style `StakeValidator` to an `StakeValidatorHash`
stakeValidatorHash
  :: forall (r :: Row Type)
   . StakeValidator
  -> Contract r (Maybe StakeValidatorHash)
stakeValidatorHash = wrapContract <<< Scripts.stakeValidatorHash

-- | Converts a Plutus-style `Validator` to a `ValidatorHash`
validatorHash
  :: forall (r :: Row Type). Validator -> Contract r (Maybe ValidatorHash)
validatorHash = wrapContract <<< Scripts.validatorHash
