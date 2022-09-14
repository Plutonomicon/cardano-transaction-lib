-- | A module for various script types, most of which are newtype wrappers
-- | over `PlutusScript`. Corresponding hashes are also included as newtype
-- | wrappers over `ScriptHash`.
module CTL.Contract.Scripts
  ( applyArgs
  , applyArgsM
  , module Address
  , module ExportQueryM
  , module ExportScripts
  , module Hash
  , module NativeScript
  , module TypedValidator
  , module TypesScripts
  ) where

import Aeson (class DecodeAeson)
import CTL.Internal.Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  ) as NativeScript
-- See Contract.Address for documentation on the various helpers, some are
-- constructive/deconstructive on the Plutus `Address` type, others are from
-- the CSL API and converted to use Plutus types.
import CTL.Contract.Address
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
import CTL.Internal.QueryM
  ( ClientError
      ( ClientHttpError
      , ClientDecodeJsonError
      , ClientEncodingError
      )
  ) as ExportQueryM
import CTL.Internal.QueryM (applyArgs) as QueryM
import CTL.Internal.Scripts
  ( mintingPolicyHash
  , scriptHash
  , stakeValidatorHash
  , validatorHash
  ) as ExportScripts
import CTL.Internal.Serialization.Hash (ScriptHash) as Hash
import CTL.Internal.Types.Scripts
  ( MintingPolicy(MintingPolicy)
  , MintingPolicyHash(MintingPolicyHash)
  , PlutusScript(PlutusScript)
  , StakeValidator(StakeValidator)
  , StakeValidatorHash(StakeValidatorHash)
  , Validator(Validator)
  , ValidatorHash(ValidatorHash)
  ) as TypesScripts
import CTL.Internal.Types.Scripts (PlutusScript)
import CTL.Internal.Types.TypedValidator
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
import CTL.Contract.Monad (Contract, wrapContract)
import Data.Either (Either, hush)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import CTL.Internal.Types.PlutusData (PlutusData)

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
