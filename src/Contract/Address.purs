-- | A module for Address-related functionality and querying own wallet.
module Contract.Address
  ( enterpriseAddressScriptHash
  , enterpriseAddressStakeValidatorHash
  , enterpriseAddressValidatorHash
  , getNetworkId
  , addressWithNetworkTagFromBech32
  , addressWithNetworkTagToBech32
  , addressFromBech32
  , addressToBech32
  , getWalletAddress
  , getWalletAddressWithNetworkTag
  , getWalletCollateral
  , module ByteArray
  , module ExportAddress
  , module ExportPubKeyHash
  , module ExportUnbalancedTransaction
  , module Hash
  , module SerializationAddress
  , module TypeAliases
  , ownPaymentPubKeyHash
  , ownPubKeyHash
  , ownStakePubKeyHash
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
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.Address
  ( enterpriseAddressScriptHash
  , enterpriseAddressStakeValidatorHash
  , enterpriseAddressValidatorHash
  ) as Address
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
import Ctl.Internal.QueryM
  ( getWalletAddresses
  , ownPaymentPubKeyHashes
  , ownPubKeyHashes
  , ownStakePubKeyHash
  ) as QueryM
import Ctl.Internal.QueryM.NetworkId (getNetworkId) as QueryM
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
import Ctl.Internal.Serialization.Hash (ScriptHash)
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
import Ctl.Internal.Types.UnbalancedTransaction
  ( PaymentPubKey(PaymentPubKey)
  ) as ExportUnbalancedTransaction
import Data.Array (head)
import Data.Maybe (Maybe)
import Data.Traversable (for, traverse)
import Effect.Exception (error)

-- | Get the `Address` of the browser wallet.
-- TODO: change this to Maybe (Array Address)
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1045
getWalletAddress
  :: forall (r :: Row Type). Contract r (Maybe Address)
getWalletAddress = do
  mbAddr <- wrapContract $ (QueryM.getWalletAddresses <#> (_ >>= head))
  for mbAddr $
    liftedM "getWalletAddress: failed to deserialize Address"
      <<< pure
      <<< toPlutusAddress

-- | Get the `AddressWithNetworkTag` of the browser wallet.
-- TODO: change this to Maybe (Array Address)
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1045
getWalletAddressWithNetworkTag
  :: forall (r :: Row Type). Contract r (Maybe AddressWithNetworkTag)
getWalletAddressWithNetworkTag = do
  mbAddr <- wrapContract $ (QueryM.getWalletAddresses <#> (_ >>= head))
  for mbAddr $
    liftedM "getWalletAddressWithNetworkTag: failed to deserialize Address"
      <<< pure
      <<< toPlutusAddressWithNetworkTag

-- | Get the collateral of the browser wallet. This collateral will vary
-- | depending on the wallet.
-- | E.g. Nami creates a hardcoded 5 Ada collateral.
-- | Throws on `Promise` rejection by wallet, returns `Nothing` if no collateral
-- | is available.
getWalletCollateral
  :: forall (r :: Row Type). Contract r (Maybe (Array TransactionUnspentOutput))
getWalletCollateral = do
  mtxUnspentOutput <- wrapContract QueryM.getWalletCollateral
  for mtxUnspentOutput $ traverse $
    liftedM
      "getWalletCollateral: failed to deserialize TransactionUnspentOutput"
      <<< pure
      <<< toPlutusTxUnspentOutput

-- | Gets the wallet `PaymentPubKeyHash` via `getWalletAddress`.
ownPaymentPubKeyHash
  :: forall (r :: Row Type). Contract r (Maybe PaymentPubKeyHash)
-- TODO: change this to Maybe (Array PaymentPubKeyHash)
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1045
ownPaymentPubKeyHash = wrapContract
  (QueryM.ownPaymentPubKeyHashes <#> (_ >>= head))

-- | Gets the wallet `PubKeyHash` via `getWalletAddress`.
ownPubKeyHash :: forall (r :: Row Type). Contract r (Maybe PubKeyHash)
-- TODO: change this to Maybe (Array PubKeyHash)
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1045
ownPubKeyHash = wrapContract (QueryM.ownPubKeyHashes <#> (_ >>= head))

-- TODO: change this to Maybe (Array StakePubKeyHash)
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1045
ownStakePubKeyHash :: forall (r :: Row Type). Contract r (Maybe StakePubKeyHash)
ownStakePubKeyHash = wrapContract QueryM.ownStakePubKeyHash

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

-- | Get the `ValidatorHash` with an Plutus `Address`
enterpriseAddressValidatorHash :: Address -> Maybe ValidatorHash
enterpriseAddressValidatorHash =
  -- Network id does not matter here (#484)
  Address.enterpriseAddressValidatorHash
    <<< fromPlutusAddress MainnetId

-- | Get the `StakeValidatorHash` with an Plutus `Address`
enterpriseAddressStakeValidatorHash :: Address -> Maybe StakeValidatorHash
enterpriseAddressStakeValidatorHash =
  -- Network id does not matter here (#484)
  Address.enterpriseAddressStakeValidatorHash
    <<< fromPlutusAddress MainnetId

-- | Get the `ScriptHash` with an Plutus `Address`
enterpriseAddressScriptHash :: Address -> Maybe ScriptHash
enterpriseAddressScriptHash =
  -- Network id does not matter here (#484)
  Address.enterpriseAddressScriptHash
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
