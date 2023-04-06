-- | A module for Address-related functionality.
module Contract.Address
  ( addressPaymentValidatorHash
  , addressStakeValidatorHash
  , getNetworkId
  , addressWithNetworkTagFromBech32
  , addressWithNetworkTagToBech32
  , addressFromBech32
  , addressToBech32
  , module X
  , module TypeAliases
  , payPubKeyHashBaseAddress
  , payPubKeyHashEnterpriseAddress
  , payPubKeyHashRewardAddress
  , pubKeyHashBaseAddress
  , pubKeyHashEnterpriseAddress
  , pubKeyHashRewardAddress
  , stakePubKeyHashRewardAddress
  , typedValidatorBaseAddress
  , typedValidatorEnterpriseAddress
  , validatorHashBaseAddress
  , validatorHashEnterpriseAddress
  ) where

import Prelude

import Contract.Monad (Contract, liftContractM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Address
  ( addressPaymentValidatorHash
  , addressStakeValidatorHash
  ) as Address
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusAddress
  , fromPlutusAddressWithNetworkTag
  , toPlutusAddress
  )
import Ctl.Internal.Plutus.Types.Address
  ( Address
  , AddressWithNetworkTag(AddressWithNetworkTag)
  )
import Ctl.Internal.Plutus.Types.Address
  ( Address
  , AddressWithNetworkTag(AddressWithNetworkTag)
  , pubKeyHashAddress
  , scriptHashAddress
  , toPubKeyHash
  , toStakingCredential
  , toValidatorHash
  ) as X
import Ctl.Internal.Scripts
  ( typedValidatorBaseAddress
  , typedValidatorEnterpriseAddress
  , validatorHashBaseAddress
  , validatorHashEnterpriseAddress
  ) as Scripts
import Ctl.Internal.Serialization.Address
  ( BlockId(BlockId)
  , ByronProtocolMagic(ByronProtocolMagic)
  , CertificateIndex(CertificateIndex)
  , NetworkId(TestnetId, MainnetId)
  , Pointer
  , Slot(Slot)
  , TransactionIndex(TransactionIndex)
  ) as X
import Ctl.Internal.Serialization.Address
  ( NetworkId(MainnetId)
  , addressBech32
  , addressNetworkId
  )
import Ctl.Internal.Serialization.Address (addressFromBech32) as SA
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash) as X
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.Aliases (Bech32String) as TypeAliases
import Ctl.Internal.Types.PaymentPubKey (PaymentPubKey(PaymentPubKey)) as X
import Ctl.Internal.Types.PubKeyHash
  ( PaymentPubKeyHash
  , PubKeyHash
  , StakePubKeyHash
  )
import Ctl.Internal.Types.PubKeyHash
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  , StakePubKeyHash(StakePubKeyHash)
  ) as X
import Ctl.Internal.Types.PubKeyHash
  ( payPubKeyHashBaseAddress
  , payPubKeyHashEnterpriseAddress
  , payPubKeyHashRewardAddress
  , pubKeyHashBaseAddress
  , pubKeyHashEnterpriseAddress
  , pubKeyHashRewardAddress
  , stakePubKeyHashRewardAddress
  ) as PubKeyHash
import Ctl.Internal.Types.Scripts (StakeValidatorHash, ValidatorHash)
import Ctl.Internal.Types.TypedValidator (TypedValidator)
import Data.Maybe (Maybe)
import Effect.Exception (error)

getNetworkId :: Contract NetworkId
getNetworkId = asks _.networkId

--------------------------------------------------------------------------------
-- Helpers via Cardano helpers, these are helpers from the CSL equivalent
-- that convert either input or output to a Plutus Address.
-- Helpers that deconstruct/construct the Plutus Address are exported under
-- `module Address`
--------------------------------------------------------------------------------

-- | Convert `Address` to `Bech32String`, using given `NetworkId` to determine
-- | Bech32 prefix.
addressWithNetworkTagToBech32 :: AddressWithNetworkTag -> Bech32String
addressWithNetworkTagToBech32 = fromPlutusAddressWithNetworkTag >>>
  addressBech32

-- | Convert `Bech32String` to `AddressWithNetworkTag`.
addressWithNetworkTagFromBech32 :: Bech32String -> Maybe AddressWithNetworkTag
addressWithNetworkTagFromBech32 str = do
  cslAddress <- SA.addressFromBech32 str
  address <- toPlutusAddress cslAddress
  let networkId = addressNetworkId cslAddress
  pure $ AddressWithNetworkTag { address, networkId }

-- | Convert `Address` to `Bech32String`, using current `NetworkId` provided by
-- | `Contract` configuration to determine the network tag.
addressToBech32 :: Address -> Contract Bech32String
addressToBech32 address = do
  networkId <- getNetworkId
  pure $ addressWithNetworkTagToBech32
    (AddressWithNetworkTag { address, networkId })

-- | Convert `Bech32String` to `Address`, asserting that the address `networkId`
-- | corresponds to the contract environment `networkId`
addressFromBech32
  :: Bech32String -> Contract Address
addressFromBech32 str = do
  networkId <- getNetworkId
  cslAddress <- liftContractM "addressFromBech32: unable to read address" $
    SA.addressFromBech32 str
  address <-
    liftContractM "addressFromBech32: unable to convert to plutus address" $
      toPlutusAddress cslAddress
  when (networkId /= addressNetworkId cslAddress)
    (throwError $ error "addressFromBech32: address has wrong NetworkId")
  pure address

-- | Get the `ValidatorHash` component of a Plutus `Address`
addressPaymentValidatorHash :: Address -> Maybe ValidatorHash
addressPaymentValidatorHash =
  -- Network id does not matter here (#484)
  Address.addressPaymentValidatorHash
    <<< fromPlutusAddress MainnetId

-- | Get the `ValidatorHash` component of a Plutus `Address`
addressStakeValidatorHash :: Address -> Maybe StakeValidatorHash
addressStakeValidatorHash =
  -- Network id does not matter here (#484)
  Address.addressStakeValidatorHash
    <<< fromPlutusAddress MainnetId

-- | Converts a Plutus `TypedValidator` to a Plutus (`BaseAddress`) `Address`
typedValidatorBaseAddress
  :: forall (a :: Type)
   . NetworkId
  -> TypedValidator a
  -> Maybe Address
typedValidatorBaseAddress networkId =
  toPlutusAddress
    <<< Scripts.typedValidatorBaseAddress networkId

-- | Converts a Plutus `TypedValidator` to a Plutus (`EnterpriseAddress`) `Address`.
-- | This is likely what you will use since Plutus currently uses
-- | `scriptHashAddress` on non-staking addresses which is invoked in
-- | `validatorAddress`
typedValidatorEnterpriseAddress
  :: forall (a :: Type)
   . NetworkId
  -> TypedValidator a
  -> Maybe Address
typedValidatorEnterpriseAddress networkId =
  toPlutusAddress
    <<< Scripts.typedValidatorEnterpriseAddress networkId

-- | Converts a Plutus `ValidatorHash` to a `Address` as a Plutus (`BaseAddress`)
-- | `Address`
validatorHashBaseAddress
  :: NetworkId -> ValidatorHash -> Maybe Address
validatorHashBaseAddress networkId =
  toPlutusAddress
    <<< Scripts.validatorHashBaseAddress networkId

-- | Converts a Plutus `ValidatorHash` to a Plutus `Address` as an
-- | `EnterpriseAddress`. This is likely what you will use since Plutus
-- | currently uses `scriptHashAddress` on non-staking addresses which is
-- | invoked in `validatorAddress`
validatorHashEnterpriseAddress
  :: NetworkId -> ValidatorHash -> Maybe Address
validatorHashEnterpriseAddress networkId =
  toPlutusAddress
    <<< Scripts.validatorHashEnterpriseAddress networkId

pubKeyHashBaseAddress
  :: NetworkId -> PubKeyHash -> StakePubKeyHash -> Maybe Address
pubKeyHashBaseAddress networkId pkh =
  toPlutusAddress
    <<< PubKeyHash.pubKeyHashBaseAddress networkId pkh

pubKeyHashRewardAddress
  :: NetworkId -> PubKeyHash -> Maybe Address
pubKeyHashRewardAddress networkId =
  toPlutusAddress
    <<< PubKeyHash.pubKeyHashRewardAddress networkId

pubKeyHashEnterpriseAddress
  :: NetworkId -> PubKeyHash -> Maybe Address
pubKeyHashEnterpriseAddress networkId =
  toPlutusAddress
    <<< PubKeyHash.pubKeyHashEnterpriseAddress networkId

payPubKeyHashRewardAddress
  :: NetworkId -> PaymentPubKeyHash -> Maybe Address
payPubKeyHashRewardAddress networkId =
  toPlutusAddress
    <<< PubKeyHash.payPubKeyHashRewardAddress networkId

payPubKeyHashBaseAddress
  :: NetworkId
  -> PaymentPubKeyHash
  -> StakePubKeyHash
  -> Maybe Address
payPubKeyHashBaseAddress networkId pkh =
  toPlutusAddress
    <<< PubKeyHash.payPubKeyHashBaseAddress networkId pkh

payPubKeyHashEnterpriseAddress
  :: NetworkId -> PaymentPubKeyHash -> Maybe Address
payPubKeyHashEnterpriseAddress networkId =
  toPlutusAddress
    <<< PubKeyHash.payPubKeyHashEnterpriseAddress networkId

stakePubKeyHashRewardAddress
  :: NetworkId -> StakePubKeyHash -> Maybe Address
stakePubKeyHashRewardAddress networkId =
  toPlutusAddress
    <<< PubKeyHash.stakePubKeyHashRewardAddress networkId
