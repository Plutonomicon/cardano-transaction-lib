module Contract.Address
  ( getNetworkId
  , addressFromBech32
  ) where

import Prelude

import Cardano.Types (Address, Bech32String, NetworkId)
import Cardano.Types.Address as Address
import Contract.Monad (Contract, liftContractM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (asks)
import Effect.Exception (error)

getNetworkId :: Contract NetworkId
getNetworkId = asks _.networkId

-- | Convert `Bech32String` to `Address`, asserting that the address `networkId`
-- | corresponds to the contract environment `networkId`
addressFromBech32
  :: Bech32String -> Contract Address
addressFromBech32 str = do
  networkId <- getNetworkId
  addr <- liftContractM "addressFromBech32: unable to read address" $
    Address.fromBech32 str
  when (networkId /= Address.getNetworkId addr)
    (throwError $ error "addressFromBech32: address has wrong NetworkId")
  pure addr
