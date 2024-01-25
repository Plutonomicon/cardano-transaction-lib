module Ctl.Internal.Address
  ( addressToOgmiosAddress
  , addressPaymentValidatorHash
  , addressStakeValidatorHash
  , ogmiosAddressToAddress
  ) where

import Prelude

import Control.Alt ((<|>))
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.Serialization.Address
  ( Address
  , addressBech32
  , addressFromBech32
  , baseAddressDelegationCred
  , baseAddressFromAddress
  , baseAddressPaymentCred
  , enterpriseAddressFromAddress
  , enterpriseAddressPaymentCred
  , stakeCredentialToScriptHash
  )
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Types.Scripts
  ( StakeValidatorHash(StakeValidatorHash)
  , ValidatorHash(ValidatorHash)
  )
import Data.Maybe (Maybe)

-- | A module for address related helpers

--------------------------------------------------------------------------------
-- Conversion between various address types
--------------------------------------------------------------------------------
-- JsonRpc2.Address is a bech32 string, so wrap to Transaction.Types.Bech32
-- | Converts an `JsonRpc2.Address` (bech32string) to internal `Address`
ogmiosAddressToAddress :: Ogmios.OgmiosAddress -> Maybe Address
ogmiosAddressToAddress = addressFromBech32

-- | Converts an (internal) `Address` to `JsonRpc2.Address` (bech32string)
addressToOgmiosAddress :: Address -> Ogmios.OgmiosAddress
addressToOgmiosAddress = addressBech32

--------------------------------------------------------------------------------
-- `Address` to `ValidatorHash`
--------------------------------------------------------------------------------

-- | Get the `ValidatorHash` of an address (base or enterprise).
-- | The value is extracted from the payment component.
addressPaymentValidatorHash :: Address -> Maybe ValidatorHash
addressPaymentValidatorHash = map ValidatorHash <<< addressPaymentScriptHash

addressPaymentScriptHash :: Address -> Maybe ScriptHash
addressPaymentScriptHash addr =
  baseAddressPaymentScriptHash addr <|> enterpriseAddressPaymentScriptHash addr

baseAddressPaymentScriptHash :: Address -> Maybe ScriptHash
baseAddressPaymentScriptHash =
  stakeCredentialToScriptHash
    <=< pure <<< baseAddressPaymentCred
    <=< baseAddressFromAddress

enterpriseAddressPaymentScriptHash :: Address -> Maybe ScriptHash
enterpriseAddressPaymentScriptHash =
  stakeCredentialToScriptHash
    <=< pure <<< enterpriseAddressPaymentCred
    <=< enterpriseAddressFromAddress

-- | Get the `StakeValidatorHash` of a base address.
-- | The value is extracted from the stake component.
addressStakeValidatorHash :: Address -> Maybe StakeValidatorHash
addressStakeValidatorHash =
  map StakeValidatorHash <<< stakeCredentialToScriptHash
    <=< pure <<< baseAddressDelegationCred
    <=< baseAddressFromAddress
