module Scripts
  ( mintingPolicyHash
  , scriptCurrencySymbol
  , scriptHash
  , stakeValidatorHash
  , typedValidatorBaseAddress
  , typedValidatorEnterpriseAddress
  , validatorHash
  , validatorHashBaseAddress
  , validatorHashEnterpriseAddress
  , nativeScriptHashEnterpriseAddress
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Effect.Aff (Aff)
import Hashing (plutusScriptHash)
import NativeScripts (NativeScriptHash)
import Plutus.Types.CurrencySymbol (CurrencySymbol, mpsSymbol)
import Serialization.Address
  ( Address
  , NetworkId
  , baseAddressToAddress
  , scriptHashCredential
  , scriptAddress
  , enterpriseAddressToAddress
  , enterpriseAddress
  )
import Serialization.Hash (ScriptHash)
import Types.Scripts
  ( MintingPolicy
  , MintingPolicyHash
  , PlutusScript
  , StakeValidator
  , StakeValidatorHash
  , Validator
  , ValidatorHash
  )
import Types.TypedValidator (TypedValidator(TypedValidator))

-- | Helpers for `PlutusScript` and `ScriptHash` newtype wrappers, separate from
-- | the data type definitions to prevent cylic dependencies.

-- | Converts a Plutus-style `TypedValidator` to an `BaseAddress`
typedValidatorBaseAddress
  :: forall (a :: Type). NetworkId -> TypedValidator a -> Address
typedValidatorBaseAddress networkId (TypedValidator typedVal) =
  baseAddressToAddress $ scriptAddress networkId $ unwrap typedVal.validatorHash

-- | Converts a Plutus-style `TypedValidator` to an `Address` as an
-- | `EnterpriseAddress`. This is likely what you will use since Plutus
-- | currently uses `scriptHashAddress` on non-staking addresses which is
-- | invoked in `validatorAddress`
typedValidatorEnterpriseAddress
  :: forall (a :: Type). NetworkId -> TypedValidator a -> Address
typedValidatorEnterpriseAddress network (TypedValidator typedVal) =
  validatorHashEnterpriseAddress network typedVal.validatorHash

-- | Converts a Plutus-style `MintingPolicy` to an `MintingPolicyHash`
mintingPolicyHash :: MintingPolicy -> Aff (Maybe MintingPolicyHash)
mintingPolicyHash = scriptHash

-- | Converts a Plutus-style `Validator` to an `ValidatorHash`
validatorHash :: Validator -> Aff (Maybe ValidatorHash)
validatorHash = scriptHash

-- | Converts a Plutus-style `ValidatorHash` to a `Address` as a `BaseAddress`
validatorHashBaseAddress :: NetworkId -> ValidatorHash -> Address
validatorHashBaseAddress networkId =
  baseAddressToAddress <<< scriptAddress networkId <<< unwrap

-- | Converts a Plutus-style `ValidatorHash` to an `Address` as an
-- | `EnterpriseAddress`. This is likely what you will use since Plutus
-- | currently uses `scriptHashAddress` on non-staking addresses which is
-- | invoked in `validatorAddress`
validatorHashEnterpriseAddress :: NetworkId -> ValidatorHash -> Address
validatorHashEnterpriseAddress network valHash =
  enterpriseAddressToAddress $
    enterpriseAddress
      { network
      , paymentCred: scriptHashCredential (unwrap valHash)
      }

nativeScriptHashEnterpriseAddress :: NetworkId -> NativeScriptHash -> Address
nativeScriptHashEnterpriseAddress network nsHash =
  validatorHashEnterpriseAddress network (wrap $ unwrap nsHash)

-- | Converts a Plutus-style `StakeValidator` to an `StakeValidatorHash`
stakeValidatorHash :: StakeValidator -> Aff (Maybe StakeValidatorHash)
stakeValidatorHash = scriptHash

-- | Converts any newtype wrapper of `PlutusScript` to a newtype wrapper
-- | of `ScriptHash`.
scriptHash
  :: forall (m :: Type) (n :: Type)
   . Newtype m PlutusScript
  => Newtype n ScriptHash
  => m
  -> Aff (Maybe n)
scriptHash = map (map wrap) <<< plutusScriptHash <<< unwrap

-- | Converts a `MintingPolicy` to a `CurrencySymbol`.
scriptCurrencySymbol :: MintingPolicy -> Aff (Maybe CurrencySymbol)
scriptCurrencySymbol = map (bindFlipped mpsSymbol) <<< mintingPolicyHash
