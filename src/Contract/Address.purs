-- | A module for Address-related functionality and querying own wallet.
module Contract.Address
  ( addressFromBech32
  , addressPaymentValidatorHash
  , addressStakeValidatorHash
  , addressToBech32
  , addressWithNetworkTagFromBech32
  , addressWithNetworkTagToBech32
  , getNetworkId
  , getWalletAddress
  , getWalletAddressWithNetworkTag
  , getWalletAddresses
  , getWalletAddressesPaginated
  , getWalletAddressesWithNetworkTag
  , getWalletCollateral
  , module ByteArray
  , module ExportAddress
  , module ExportPubKeyHash
  , module ExportUnbalancedTransaction
  , module Hash
  , module SerializationAddress
  , module TypeAliases
  , ownPaymentPubKeyHash
  , ownPaymentPubKeysHashes
  , ownStakePubKeyHash
  , ownStakePubKeysHashes
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

import Contract.Monad (Contract, liftContractM, liftedM, wrapContract)
import Contract.Prelude (liftM)
import Contract.Value (Coin, Value, getLovelace)
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.Address
  ( addressPaymentValidatorHash
  , addressStakeValidatorHash
  ) as Address
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusAddress
  , fromPlutusAddressWithNetworkTag
  , toPlutusAddress
  , toPlutusAddressWithNetworkTag
  , toPlutusTxUnspentOutput
  )
import Ctl.Internal.Plutus.Conversion.Value (fromPlutusCoin, fromPlutusValue)
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
  ) as ExportAddress
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import Ctl.Internal.QueryM
  ( getNetworkId
  , getWalletAddresses
  , ownPaymentPubKeyHashes
  , ownStakePubKeysHashes
  ) as QueryM
import Ctl.Internal.QueryM.Utxos (getWalletCollateral) as QueryM
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
  ) as SerializationAddress
import Ctl.Internal.Serialization.Address
  ( NetworkId(MainnetId)
  , addressBech32
  , addressNetworkId
  )
import Ctl.Internal.Serialization.Address (addressFromBech32) as SA
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash) as Hash
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.Aliases (Bech32String) as TypeAliases
import Ctl.Internal.Types.ByteArray (ByteArray) as ByteArray
import Ctl.Internal.Types.PubKeyHash
  ( PaymentPubKeyHash
  , PubKeyHash
  , StakePubKeyHash
  )
import Ctl.Internal.Types.PubKeyHash
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  , StakePubKeyHash(StakePubKeyHash)
  ) as ExportPubKeyHash
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
import Ctl.Internal.Types.UnbalancedTransaction (PaymentPubKey(PaymentPubKey)) as ExportUnbalancedTransaction
import Ctl.Internal.Wallet.Cip30 (Paginate)
import Data.Array (head)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Traversable (for, traverse)
import Effect.Exception (error)
import Prim.TypeError (class Warn, Text)

-- | Get an `Address` of the browser wallet.
getWalletAddress
  :: forall (r :: Row Type)
   . Warn
       ( Text
           "This function returns only one `Adress` even in case multiple `Adress`es are available. Use `getWalletAdresses` instead"
       )
  => Contract r (Maybe Address)
getWalletAddress = head <$> getWalletAddresses

-- | Get all the `Address`es of the browser wallet.
getWalletAddresses
  :: forall (r :: Row Type). Contract r (Array Address)
getWalletAddresses = getWalletAddressesGeneral Nothing

-- | Get all the `Address`es of the browser wallet with pagination.
getWalletAddressesPaginated
  :: Paginate -> forall (r :: Row Type). Contract r (Array Address)
getWalletAddressesPaginated paginate = getWalletAddressesGeneral $ Just paginate

getWalletAddressesGeneral
  :: Maybe Paginate -> forall (r :: Row Type). Contract r (Array Address)
getWalletAddressesGeneral paginate = do
  addresses <- wrapContract $ QueryM.getWalletAddresses paginate
  traverse
    ( liftM
        (error "getWalletAddresses: failed to deserialize `Address`")
        <<< toPlutusAddress
    )
    addresses

-- | Get an `AddressWithNetworkTag` of the browser wallet.
getWalletAddressWithNetworkTag
  :: forall (r :: Row Type)
   . Warn
       ( Text
           "This function returns only one `AddressWithNetworkTag` even in case multiple `AddressWithNetworkTag` are available. Use `getWalletAddressesWithNetworkTag` instead"
       )
  => Contract r (Maybe AddressWithNetworkTag)
getWalletAddressWithNetworkTag = head <$> getWalletAddressesWithNetworkTag

-- | Get all the `AddressWithNetworkTag` of the browser wallet discarding errors.
getWalletAddressesWithNetworkTag
  :: forall (r :: Row Type). Contract r (Array AddressWithNetworkTag)
getWalletAddressesWithNetworkTag = do
  addresses <- wrapContract $ QueryM.getWalletAddresses Nothing
  traverse
    ( liftM
        ( error
            "getWalletAddressesWithNetworkTag: failed to deserialize `Address`"
        )
        <<< toPlutusAddressWithNetworkTag
    )
    addresses

-- | Get the collateral of the browser wallet. This collateral will vary
-- | depending on the wallet.
-- | E.g. Nami creates a hardcoded 5 Ada collateral.
-- | Throws on `Promise` rejection by wallet, returns `Nothing` if no collateral
-- | is available.
getWalletCollateral
  :: forall (r :: Row Type)
   . Coin
  -> Contract r (Maybe (Array TransactionUnspentOutput))
getWalletCollateral amount = do
  mtxUnspentOutput <- wrapContract $ QueryM.getWalletCollateral $
    fromPlutusCoin amount
  for mtxUnspentOutput $ traverse $
    liftedM
      "getWalletCollateral: failed to deserialize TransactionUnspentOutput"
      <<< pure
      <<< toPlutusTxUnspentOutput

-- | Gets a wallet `PaymentPubKeyHash` via `getWalletAddresses`.
ownPaymentPubKeyHash
  :: forall (r :: Row Type)
   . Warn
       ( Text
           "This function returns only one `PaymentPubKeyHash` even in case multiple `PaymentPubKeysHash`es are available. Use `ownPaymentPubKeysHashes` instead"
       )
  => Contract r (Maybe PaymentPubKeyHash)
ownPaymentPubKeyHash = head <$> ownPaymentPubKeysHashes

-- | Gets all wallet `PaymentPubKeyHash`es via `getWalletAddresses`.
ownPaymentPubKeysHashes
  :: forall (r :: Row Type). Contract r (Array PaymentPubKeyHash)
ownPaymentPubKeysHashes = wrapContract $ QueryM.ownPaymentPubKeyHashes Nothing

ownStakePubKeyHash
  :: forall (r :: Row Type)
   . Warn
       ( Text
           "This function returns only one `StakePubKeyHash` even in case multiple `StakePubKeysHash`es are available. Use `ownStakePubKeysHashes` instead"
       )
  => Contract r (Maybe StakePubKeyHash)
ownStakePubKeyHash = join <<< head <$> ownStakePubKeysHashes

ownStakePubKeysHashes
  :: forall (r :: Row Type). Contract r (Array (Maybe StakePubKeyHash))
ownStakePubKeysHashes = wrapContract $ QueryM.ownStakePubKeysHashes Nothing

getNetworkId
  :: forall (r :: Row Type). Contract r NetworkId
getNetworkId = wrapContract QueryM.getNetworkId

--------------------------------------------------------------------------------
-- Helpers via Cardano helpers, these are helpers from the CSL equivalent
-- that converts either input or output to a Plutus Address.
-- Helpers by deconstructing/constructing the Plutus Address are exported under
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
addressToBech32 :: forall (r :: Row Type). Address -> Contract r Bech32String
addressToBech32 address = do
  networkId <- getNetworkId
  pure $ addressWithNetworkTagToBech32
    (AddressWithNetworkTag { address, networkId })

-- | Convert `Bech32String` to `Address`, asserting that the address `networkId`
-- | corresponds to the contract environment `networkId`
addressFromBech32
  :: forall (r :: Row Type). Bech32String -> Contract r Address
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
