module Ctl.Internal.Address
  ( addressToOgmiosAddress
  , enterpriseAddressMintingPolicyHash
  , enterpriseAddressScriptHash
  , enterpriseAddressStakeValidatorHash
  , enterpriseAddressValidatorHash
  , ogmiosAddressToAddress
  ) where

import Prelude

import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.Serialization.Address
  ( Address
  , addressBech32
  , addressFromBech32
  , enterpriseAddressFromAddress
  , enterpriseAddressPaymentCred
  , stakeCredentialToScriptHash
  )
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Types.Scripts
  ( MintingPolicyHash(MintingPolicyHash)
  , StakeValidatorHash(StakeValidatorHash)
  , ValidatorHash(ValidatorHash)
  )
import Data.Maybe (Maybe)

-- | A module for address related helpers

--------------------------------------------------------------------------------
-- Conversion between various address types
--------------------------------------------------------------------------------
-- JsonWsp.Address is a bech32 string, so wrap to Transaction.Types.Bech32
-- | Converts an `JsonWsp.Address` (bech32string) to internal `Address`
ogmiosAddressToAddress :: Ogmios.OgmiosAddress -> Maybe Address
ogmiosAddressToAddress = addressFromBech32

-- | Converts an (internal) `Address` to `JsonWsp.Address` (bech32string)
addressToOgmiosAddress :: Address -> Ogmios.OgmiosAddress
addressToOgmiosAddress = addressBech32

--------------------------------------------------------------------------------
-- `Address` to `ScriptHash`
--------------------------------------------------------------------------------
-- | Get the `ScriptHash` with an internal `Address`
enterpriseAddressScriptHash :: Address -> Maybe ScriptHash
enterpriseAddressScriptHash =
  stakeCredentialToScriptHash
    <=< pure <<< enterpriseAddressPaymentCred
    <=< enterpriseAddressFromAddress

-- | Get the `ValidatorHash` with an internal `Address`
enterpriseAddressValidatorHash :: Address -> Maybe ValidatorHash
enterpriseAddressValidatorHash =
  map ValidatorHash <<< enterpriseAddressScriptHash

-- | Get the `MintingPolicyHash` with an internal `Address`
enterpriseAddressMintingPolicyHash :: Address -> Maybe MintingPolicyHash
enterpriseAddressMintingPolicyHash =
  map MintingPolicyHash <<< enterpriseAddressScriptHash

-- | Get the `StakeValidatorHash` with an internal `Address`
enterpriseAddressStakeValidatorHash :: Address -> Maybe StakeValidatorHash
enterpriseAddressStakeValidatorHash =
  map StakeValidatorHash <<< enterpriseAddressScriptHash
