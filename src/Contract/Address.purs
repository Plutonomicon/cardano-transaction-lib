-- | A module for Address-related functionality and querying own wallet.
module Contract.Address
  ( addressPaymentValidatorHash
  , addressStakeValidatorHash
  , getNetworkId
  , addressWithNetworkTagFromBech32
  , addressWithNetworkTagToBech32
  , addressFromBech32
  , addressToBech32
  , getWalletAddress
  , getWalletAddresses
  , getWalletAddressWithNetworkTag
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

import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Prelude (liftM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Address
  ( addressPaymentValidatorHash
  , addressStakeValidatorHash
  ) as Address
import Ctl.Internal.Contract.Wallet
  ( getWalletAddresses
  , getWalletCollateral
  , ownPaymentPubKeyHashes
  , ownStakePubKeysHashes
  ) as Contract
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusAddress
  , fromPlutusAddressWithNetworkTag
  , toPlutusAddress
  , toPlutusAddressWithNetworkTag
  , toPlutusTxUnspentOutput
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
  ) as ExportAddress
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
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
import Data.Array (head)
import Data.Maybe (Maybe)
import Data.Traversable (for, traverse)
import Effect.Exception (error)
import Prim.TypeError (class Warn, Text)

-- | Get an `Address` of the browser wallet.
getWalletAddress
  :: Warn
       ( Text
           "This function returns only one `Adress` even in case multiple `Adress`es are available. Use `getWalletAdresses` instead"
       )
  => Contract (Maybe Address)
getWalletAddress = head <$> getWalletAddresses

-- | Get all the `Address`es of the browser wallet.
getWalletAddresses :: Contract (Array Address)
getWalletAddresses = do
  addresses <- Contract.getWalletAddresses
  traverse
    ( liftM
        (error "getWalletAddresses: failed to deserialize `Address`")
        <<< toPlutusAddress
    )
    addresses

-- | Get an `AddressWithNetworkTag` of the browser wallet.
getWalletAddressWithNetworkTag
  :: Warn
       ( Text
           "This function returns only one `AddressWithNetworkTag` even in case multiple `AddressWithNetworkTag` are available. Use `getWalletAddressesWithNetworkTag` instead"
       )
  => Contract (Maybe AddressWithNetworkTag)
getWalletAddressWithNetworkTag = head <$> getWalletAddressesWithNetworkTag

-- | Get all the `AddressWithNetworkTag`s of the browser wallet discarding errors.
getWalletAddressesWithNetworkTag :: Contract (Array AddressWithNetworkTag)
getWalletAddressesWithNetworkTag = do
  addresses <- Contract.getWalletAddresses
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
-- | E.g. Nami creates a hard-coded 5 Ada collateral.
-- | Throws on `Promise` rejection by wallet, returns `Nothing` if no collateral
-- | is available.
getWalletCollateral
  :: Contract (Maybe (Array TransactionUnspentOutput))
getWalletCollateral = do
  mtxUnspentOutput <- Contract.getWalletCollateral
  for mtxUnspentOutput $ traverse $
    liftedM
      "getWalletCollateral: failed to deserialize TransactionUnspentOutput"
      <<< pure
      <<< toPlutusTxUnspentOutput

-- | Gets a wallet `PaymentPubKeyHash` via `getWalletAddresses`.
ownPaymentPubKeyHash
  :: Warn
       ( Text
           "This function returns only one `PaymentPubKeyHash` even in case multiple `PaymentPubKeysHash`es are available. Use `ownPaymentPubKeysHashes` instead"
       )
  => Contract (Maybe PaymentPubKeyHash)
ownPaymentPubKeyHash = head <$> ownPaymentPubKeysHashes

-- | Gets all wallet `PaymentPubKeyHash`es via `getWalletAddresses`.
ownPaymentPubKeysHashes :: Contract (Array PaymentPubKeyHash)
ownPaymentPubKeysHashes = Contract.ownPaymentPubKeyHashes

ownStakePubKeyHash
  :: Warn
       ( Text
           "This function returns only one `StakePubKeyHash` even in case multiple `StakePubKeysHash`es are available. Use `ownStakePubKeysHashes` instead"
       )
  => Contract (Maybe StakePubKeyHash)
ownStakePubKeyHash = join <<< head <$> ownStakePubKeysHashes

ownStakePubKeysHashes :: Contract (Array (Maybe StakePubKeyHash))
ownStakePubKeysHashes = Contract.ownStakePubKeysHashes

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
