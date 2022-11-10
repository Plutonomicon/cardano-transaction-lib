module Ctl.Internal.Address
  ( addressToOgmiosAddress
  , addressValidatorHash
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
  , enterpriseAddressFromAddress
  , enterpriseAddressPaymentCred
  , stakeCredentialToScriptHash
  )
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Types.Scripts (ValidatorHash(ValidatorHash))
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
-- `Address` to `ValidatorHash`
--------------------------------------------------------------------------------

enterpriseAddressScriptHash :: Address -> Maybe ScriptHash
enterpriseAddressScriptHash =
  stakeCredentialToScriptHash
    <=< pure <<< enterpriseAddressPaymentCred
    <=< enterpriseAddressFromAddress

baseAddressScriptHash :: Address -> Maybe ScriptHash
baseAddressScriptHash =
  stakeCredentialToScriptHash
    <=< pure <<< baseAddressDelegationCred
    <=< baseAddressFromAddress

addressScriptHash :: Address -> Maybe ScriptHash
addressScriptHash addr =
  baseAddressScriptHash addr <|> enterpriseAddressScriptHash addr

-- | Get the `ValidatorHash` of an address (base or enterprise).
addressValidatorHash :: Address -> Maybe ValidatorHash
addressValidatorHash = map ValidatorHash <<< addressScriptHash
