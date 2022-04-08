module Address
  ( addressToOgmiosAddress
  , enterpriseAddressMintingPolicyHash
  , enterpriseAddressScriptHash
  , enterpriseAddressStakeValidatorHash
  , enterpriseAddressValidatorHash
  , getNetworkId
  , ogmiosAddressToAddress
  ) where

import Prelude

import Control.Monad.Reader.Class (asks)
import Data.Maybe (Maybe)
import QueryM (QueryM)
import QueryM.Ogmios as Ogmios
import Types.Scripts
  ( MintingPolicyHash(MintingPolicyHash)
  , StakeValidatorHash(StakeValidatorHash)
  , ValidatorHash(ValidatorHash)
  )
import Serialization.Address
  ( Address
  , NetworkId
  , addressBech32
  , addressFromBech32
  , enterpriseAddressFromAddress
  , enterpriseAddressPaymentCred
  , stakeCredentialToScriptHash
  )
import Serialization.Hash (ScriptHash)

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

--------------------------------------------------------------------------------
-- NetworkId
--------------------------------------------------------------------------------
getNetworkId :: QueryM NetworkId
getNetworkId = asks _.networkId
