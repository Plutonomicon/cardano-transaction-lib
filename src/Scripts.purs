module Scripts
  ( mintingPolicyHash
  , scriptCurrencySymbol
  , scriptHash
  , stakeValidatorHash
  , typedValidatorBaseAddress
  , typedValidatorEnterpriseAddress
  , validatorAddress
  , validatorBaseAddress
  , validatorHash
  , validatorHashAddress
  , validatorHashBaseAddress
  ) where

import Prelude
import Data.Either (hush)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Newtype (class Newtype, unwrap)
import QueryM (QueryM, hashScript)
import Serialization.Address
  ( Address
  , BaseAddress
  , ByronAddress
  , EnterpriseAddress
  , PointerAddress
  , RewardAddress
  , NetworkId
  , addressFromBytes
  , baseAddressFromBytes
  , baseAddressToAddress
  , enterpriseAddressFromBytes
  , byronAddressFromBytes
  , scriptHashCredential
  , pointerAddressFromBytes
  , rewardAddress
  , rewardAddressFromBytes
  , scriptAddress
  , enterpriseAddressToAddress
  , enterpriseAddress
  )
import Serialization.Hash
  ( ScriptHash
  , scriptHashToBytes
  )
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
import Types.Value (CurrencySymbol, mpsSymbol)

-- | Helpers for `PlutusScript` and `ScriptHash` newtype wrappers, separate from
-- | the data type definitions to prevent cylic dependencies.

-- | Converts a Plutus-style `Validator` to a `BaseAddress`
validatorBaseAddress :: Validator -> QueryM (Maybe BaseAddress)
validatorBaseAddress val =
  map (scriptHashToBytes <<< unwrap) <$> validatorHash val >>=
    maybe Nothing baseAddressFromBytes >>> pure

-- | Converts a Plutus-style `Validator` to an `Address`
validatorAddress :: Validator -> QueryM (Maybe Address)
validatorAddress val =
  map (scriptHashToBytes <<< unwrap) <$> validatorHash val >>=
    maybe Nothing addressFromBytes >>> pure

-- | Converts a Plutus-style `TypedValidator` to an `Address` as a `BaseAddress`
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
  enterpriseAddressToAddress $
    enterpriseAddress
      { network
      , paymentCred: scriptHashCredential (unwrap typedVal.validatorHash)
      }

-- | Converts a Plutus-style `MintingPolicy` to an `MintingPolicyHash`
mintingPolicyHash :: MintingPolicy -> QueryM (Maybe MintingPolicyHash)
mintingPolicyHash = scriptHash

-- | Converts a Plutus-style `Validator` to an `ValidatorHash`
validatorHash :: Validator -> QueryM (Maybe ValidatorHash)
validatorHash = scriptHash

-- | Converts a Plutus-style `ValidatorHash` to a `BaseAddress`
validatorHashBaseAddress :: NetworkId -> ValidatorHash -> BaseAddress
validatorHashBaseAddress networkId = scriptAddress networkId <<< unwrap

-- | Converts a Plutus-style `ValidatorHash` to an `Address`
validatorHashAddress :: NetworkId -> ValidatorHash -> Address
validatorHashAddress networkId =
  baseAddressToAddress <<< validatorHashBaseAddress networkId

-- | Converts a Plutus-style `StakeValidator` to an `StakeValidatorHash`
stakeValidatorHash :: StakeValidator -> QueryM (Maybe StakeValidatorHash)
stakeValidatorHash = scriptHash

-- | Converts any newtype wrapper of `PlutusScript` to a newtype wrapper
-- | of `ScriptHash`.
scriptHash
  :: forall (m :: Type) (n :: Type)
   . Newtype m PlutusScript
  => Newtype n ScriptHash
  => m
  -> QueryM (Maybe n)
scriptHash = map hush <<< hashScript

-- | Converts a `MintingPolicy` to a `CurrencySymbol`.
scriptCurrencySymbol :: MintingPolicy -> QueryM (Maybe CurrencySymbol)
scriptCurrencySymbol mp =
  mintingPolicyHash mp >>= maybe Nothing mpsSymbol >>> pure

