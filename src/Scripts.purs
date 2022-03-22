module Scripts
  ( mintingPolicyHash
  , scriptCurrencySymbol
  , scriptHash
  , stakeValidatorHash
  , typedValidatorAddress
  , typedValidatorBaseAddress
  , validatorAddress
  , validatorBaseAddress
  , validatorHash
  , validatorHashAddress
  , validatorHashBaseAddress
  ) where

import Prelude
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(MaybeT))
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import QueryM (QueryM)
import Serialization.Address
  ( Address
  , BaseAddress
  , NetworkId
  , addressFromBytes
  , baseAddressFromBytes
  , baseAddressToAddress
  , scriptAddress
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
import Undefined (undefined)

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

-- | Converts a Plutus-style `TypedValidator` to an `BaseAddress`
typedValidatorBaseAddress
  :: forall (a :: Type). NetworkId -> TypedValidator a -> BaseAddress
typedValidatorBaseAddress networkId (TypedValidator typedVal) =
  scriptAddress networkId $ unwrap typedVal.validatorHash

-- | Converts a Plutus-style `TypedValidator` to an `Address`
typedValidatorAddress
  :: forall (a :: Type). NetworkId -> TypedValidator a -> Address
typedValidatorAddress networkId =
  baseAddressToAddress <<< typedValidatorBaseAddress networkId

-- | Converts a Plutus-style `MintingPolicy` to an `MintingPolicyHash`
mintingPolicyHash :: MintingPolicy -> QueryM (Maybe MintingPolicyHash)
mintingPolicyHash = plutusScriptHash

-- | Converts a Plutus-style `Validator` to an `ValidatorHash`
validatorHash :: Validator -> QueryM (Maybe ValidatorHash)
validatorHash = plutusScriptHash

-- | Converts a Plutus-style `ValidatorHash` to a `BaseAddress`
validatorHashBaseAddress :: NetworkId -> ValidatorHash -> BaseAddress
validatorHashBaseAddress networkId = scriptAddress networkId <<< unwrap

-- | Converts a Plutus-style `ValidatorHash` to an `Address`
validatorHashAddress :: NetworkId -> ValidatorHash -> Address
validatorHashAddress networkId =
  baseAddressToAddress <<< validatorHashBaseAddress networkId

-- | Converts a Plutus-style `StakeValidator` to an `Address`
stakeValidatorHash :: StakeValidator -> QueryM (Maybe StakeValidatorHash)
stakeValidatorHash = plutusScriptHash

plutusScriptHash
  :: forall (m :: Type) (n :: Type)
   . Newtype m PlutusScript
  => Newtype n ScriptHash
  => m
  -> QueryM (Maybe n)
plutusScriptHash = map (map wrap) <<< scriptHash <<< unwrap

-- | Converts a `PlutusScript` to a `ScriptHash`.
scriptHash :: PlutusScript -> QueryM (Maybe ScriptHash)
scriptHash = undefined

scriptCurrencySymbol :: MintingPolicy -> QueryM (Maybe CurrencySymbol)
scriptCurrencySymbol mp =
  mintingPolicyHash mp >>= maybe Nothing mpsSymbol >>> pure