module Contract.Address
  ( getNetworkId
  , addressFromBech32
  , mkAddress
  , module X
  ) where

import Prelude

import Cardano.Types
  ( Address
  , Bech32String
  , NetworkId
  , PaymentCredential
  , StakeCredential
  )
import Cardano.Types
  ( Ed25519KeyHash
  , PaymentPubKeyHash(PaymentPubKeyHash)
  , StakePubKeyHash(StakePubKeyHash)
  ) as X
import Cardano.Types.Address
  ( Address
      ( BaseAddress
      , ByronAddress
      , EnterpriseAddress
      , RewardAddress
      )
  ) as X
import Cardano.Types.Address (mkPaymentAddress)
import Cardano.Types.Address as Address
import Contract.Monad (Contract, liftContractM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (asks)
import Data.Maybe (Maybe)
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

mkAddress
  :: PaymentCredential -> Maybe StakeCredential -> Contract Address
mkAddress pc msc = do
  networkId <- getNetworkId
  pure $ mkPaymentAddress networkId pc msc
