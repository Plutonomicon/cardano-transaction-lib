-- | A module for various script types, most of which are newtype wrappers
-- | over `PlutusScript`. Corresponding hashes are also included as newtype
-- | wrappers over `ScriptHash`.
module Contract.Scripts
  ( applyArgs
  , applyArgsM
  , module Address
  , module ExportQueryM
  , module ExportScripts
  , module Hash
  , module TypedValidator
  , module TypesScripts
  ) where

import Prelude

import Aeson (class DecodeAeson)
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
import Contract.Monad (Contract, wrapContract)
import Data.Either (Either, hush)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import QueryM
  ( ClientError
      ( ClientHttpError
      , ClientDecodeJsonError
      , ClientEncodingError
      )
  ) as ExportQueryM
import QueryM (applyArgs) as QueryM
import Scripts
  ( mintingPolicyHash
  , scriptHash
  , stakeValidatorHash
  , validatorHash
  ) as ExportScripts
import Serialization.Hash (ScriptHash) as Hash
import Types.PlutusData (PlutusData)
import Types.Scripts
  ( MintingPolicy(MintingPolicy)
  , MintingPolicyHash(MintingPolicyHash)
  , PlutusScript(PlutusScript)
  , StakeValidator(StakeValidator)
  , StakeValidatorHash(StakeValidatorHash)
  , Validator(Validator)
  , ValidatorHash(ValidatorHash)
  ) as TypesScripts
import Types.Scripts (PlutusScript)
import Types.TypedValidator
  ( class DatumType
  , class RedeemerType
  , class ValidatorTypes
  , TypedValidator(TypedValidator)
  , ValidatorType
  , WrappedValidatorType
  , forwardingMintingPolicy
  , generalise
  , typedValidatorHash
  , typedValidatorScript
  ) as TypedValidator

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
