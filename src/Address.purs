module Address
  ( addressMintingPolicyHash
  , addressScriptHash
  , addressStakeValidatorHash
  , addressToOgmiosAddress
  , addressValidatorHash
  , ogmiosAddressToAddress
  ) where

import Prelude

import Data.Maybe (Maybe)
import Types.JsonWsp as JsonWsp
import Types.Scripts
  ( MintingPolicyHash(MintingPolicyHash)
  , StakeValidatorHash(StakeValidatorHash)
  , ValidatorHash(ValidatorHash)
  )
import Serialization.Address
  ( Address
  , addressBech32
  , addressFromBech32
  , addressPaymentCred
  , stakeCredentialToScriptHash
  )
import Serialization.Hash (ScriptHash)

-- | A module for address related helpers

--------------------------------------------------------------------------------
-- Conversion between various address types
--------------------------------------------------------------------------------
-- JsonWsp.Address is a bech32 string, so wrap to Transaction.Types.Bech32
-- | Converts an `JsonWsp.Address` (bech32string) to internal `Address`
ogmiosAddressToAddress :: JsonWsp.OgmiosAddress -> Maybe Address
ogmiosAddressToAddress = addressFromBech32

-- | Converts an (internal) `Address` to `JsonWsp.Address` (bech32string)
addressToOgmiosAddress :: Address -> JsonWsp.OgmiosAddress
addressToOgmiosAddress = addressBech32

--------------------------------------------------------------------------------
-- `Address` to `ScriptHash`
--------------------------------------------------------------------------------
-- | Get the `ScriptHash` with an internal `Address`
addressScriptHash :: Address -> Maybe ScriptHash
addressScriptHash = stakeCredentialToScriptHash <=< addressPaymentCred

-- | Get the `ValidatorHash` with an internal `Address`
addressValidatorHash :: Address -> Maybe ValidatorHash
addressValidatorHash = map ValidatorHash <<< addressScriptHash

-- | Get the `MintingPolicyHash` with an internal `Address`
addressMintingPolicyHash :: Address -> Maybe MintingPolicyHash
addressMintingPolicyHash = map MintingPolicyHash <<< addressScriptHash

-- | Get the `StakeValidatorHash` with an internal `Address`
addressStakeValidatorHash :: Address -> Maybe StakeValidatorHash
addressStakeValidatorHash = map StakeValidatorHash <<< addressScriptHash
