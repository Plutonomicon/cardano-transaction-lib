-- | A module for Address-related functionality and querying own wallet.
module Contract.Address
  ( enterpriseAddressScriptHash
  , enterpriseAddressStakeValidatorHash
  , enterpriseAddressValidatorHash
  , getNetworkId
  , getWalletAddress
  , getWalletCollateral
  , module ByteArray
  , module ExportAddress
  , module ExportUnbalancedTransaction
  , module Hash
  , module PubKeyHash
  , module SerializationAddress
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

import Address
  ( enterpriseAddressScriptHash
  , enterpriseAddressStakeValidatorHash
  , enterpriseAddressValidatorHash
  , getNetworkId
  ) as Address
import Contract.Monad (Contract, wrapContract, liftedM)
import Data.Maybe (Maybe)
import Data.Traversable (for)
import Plutus.FromPlutusType (fromPlutusType)
import Plutus.ToPlutusType (toPlutusType)
import Plutus.Types.Address (Address)
-- The helpers under Plutus.Type.Address deconstruct/construct the Plutus
-- `Address` directly, instead of those defined in this module, which uses
-- the CSL helpers and redefines using Plutus-style types.
import Plutus.Types.Address
  ( Address
  , pubKeyHashAddress
  , scriptHashAddress
  , toPubKeyHash
  , toValidatorHash
  , toStakingCredential
  ) as ExportAddress
import Plutus.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import QueryM
  ( getWalletAddress
  , getWalletCollateral
  , ownPaymentPubKeyHash
  , ownPubKeyHash
  , ownStakePubKeyHash
  ) as QueryM
import Scripts
  ( typedValidatorBaseAddress
  , typedValidatorEnterpriseAddress
  , validatorHashBaseAddress
  , validatorHashEnterpriseAddress
  ) as Scripts
import Serialization.Address
  ( Slot(Slot)
  , BlockId(BlockId)
  , TransactionIndex(TransactionIndex)
  , CertificateIndex(CertificateIndex)
  , Pointer
  , ByronProtocolMagic(ByronProtocolMagic)
  , NetworkId(TestnetId, MainnetId)
  ) as SerializationAddress
import Serialization.Address (NetworkId)
import Serialization.Hash (Ed25519KeyHash) as Hash
import Serialization.Hash (ScriptHash)
import Types.ByteArray (ByteArray) as ByteArray
import Types.PubKeyHash (PubKeyHash)
import Types.PubKeyHash (PubKeyHash(PubKeyHash)) as PubKeyHash
import Types.Scripts
  ( StakeValidatorHash
  , ValidatorHash
  )
import Types.TypedValidator (TypedValidator)
import Types.UnbalancedTransaction (StakePubKeyHash, PaymentPubKeyHash)
import Types.UnbalancedTransaction
  ( PaymentPubKey(PaymentPubKey)
  , PaymentPubKeyHash(PaymentPubKeyHash)
  , ScriptOutput(ScriptOutput)
  , StakePubKeyHash(StakePubKeyHash)
  ) as ExportUnbalancedTransaction
import Types.UnbalancedTransaction
  ( payPubKeyHashBaseAddress
  , payPubKeyHashRewardAddress
  , payPubKeyHashEnterpriseAddress
  , pubKeyHashBaseAddress
  , pubKeyHashEnterpriseAddress
  , pubKeyHashRewardAddress
  , stakePubKeyHashRewardAddress
  ) as UnbalancedTransaction

-- | Get the `Address` of the browser wallet.
getWalletAddress :: forall (r :: Row Type). Contract r (Maybe Address)
getWalletAddress = do
  mbAddr <- wrapContract $ QueryM.getWalletAddress
  for mbAddr $
    liftedM "getWalletAddress: failed to deserialize Address"
      <<< wrapContract
      <<< pure
      <<< toPlutusType

-- | Get the collateral of the browser wallet. This collateral will vary
-- | depending on the wallet.
-- | E.g. Nami creates a hardcoded 5 Ada collateral.
getWalletCollateral
  :: forall (r :: Row Type). Contract r (Maybe TransactionUnspentOutput)
getWalletCollateral = do
  mtxUnspentOutput <- wrapContract QueryM.getWalletCollateral
  for mtxUnspentOutput $
    liftedM
      "getWalletCollateral: failed to deserialize TransactionUnspentOutput"
      <<< wrapContract
      <<< pure
      <<< toPlutusType

-- | Gets the wallet `PaymentPubKeyHash` via `getWalletAddress`.
ownPaymentPubKeyHash
  :: forall (r :: Row Type). Contract r (Maybe PaymentPubKeyHash)
ownPaymentPubKeyHash = wrapContract QueryM.ownPaymentPubKeyHash

-- | Gets the wallet `PubKeyHash` via `getWalletAddress`.
ownPubKeyHash :: forall (r :: Row Type). Contract r (Maybe PubKeyHash)
ownPubKeyHash = wrapContract QueryM.ownPubKeyHash

ownStakePubKeyHash :: forall (r :: Row Type). Contract r (Maybe StakePubKeyHash)
ownStakePubKeyHash = wrapContract QueryM.ownStakePubKeyHash

-- | Gets the wallet `PubKeyHash` via `getWalletAddress`.
getNetworkId
  :: forall (r :: Row Type). Contract r NetworkId
getNetworkId = wrapContract Address.getNetworkId

--------------------------------------------------------------------------------
-- Helpers via Cardano helpers, these are helpers from the CSL equivalent
-- that converts either input or output to a Plutus Address.
-- Helpers by deconstructing/constructing the Plutus Address are exported under
-- `module Address`
--------------------------------------------------------------------------------

-- | Get the `ValidatorHash` with an Plutus `Address`
enterpriseAddressValidatorHash :: Address -> Maybe ValidatorHash
enterpriseAddressValidatorHash =
  Address.enterpriseAddressValidatorHash <=< fromPlutusType

-- | Get the `StakeValidatorHash` with an Plutus `Address`
enterpriseAddressStakeValidatorHash :: Address -> Maybe StakeValidatorHash
enterpriseAddressStakeValidatorHash =
  Address.enterpriseAddressStakeValidatorHash <=< fromPlutusType

-- | Get the `ScriptHash` with an Plutus `Address`
enterpriseAddressScriptHash :: Address -> Maybe ScriptHash
enterpriseAddressScriptHash =
  Address.enterpriseAddressScriptHash <=< fromPlutusType

-- | Converts a Plutus `TypedValidator` to a Plutus (`BaseAddress`) `Address`
typedValidatorBaseAddress
  :: forall (a :: Type)
   . NetworkId
  -> TypedValidator a
  -> Maybe Address
typedValidatorBaseAddress networkId =
  toPlutusType <<< Scripts.typedValidatorBaseAddress networkId

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
  toPlutusType <<< Scripts.typedValidatorEnterpriseAddress networkId

-- | Converts a Plutus `ValidatorHash` to a `Address` as a Plutus (`BaseAddress`)
-- | `Address`
validatorHashBaseAddress :: NetworkId -> ValidatorHash -> Maybe Address
validatorHashBaseAddress networkId =
  toPlutusType <<< Scripts.validatorHashBaseAddress networkId

-- | Converts a Plutus `ValidatorHash` to a Plutus `Address` as an
-- | `EnterpriseAddress`. This is likely what you will use since Plutus
-- | currently uses `scriptHashAddress` on non-staking addresses which is
-- | invoked in `validatorAddress`
validatorHashEnterpriseAddress :: NetworkId -> ValidatorHash -> Maybe Address
validatorHashEnterpriseAddress networkId =
  toPlutusType <<< Scripts.validatorHashEnterpriseAddress networkId

pubKeyHashBaseAddress
  :: NetworkId -> PubKeyHash -> StakePubKeyHash -> Maybe Address
pubKeyHashBaseAddress networkId pkh =
  toPlutusType <<< UnbalancedTransaction.pubKeyHashBaseAddress networkId pkh

pubKeyHashRewardAddress :: NetworkId -> PubKeyHash -> Maybe Address
pubKeyHashRewardAddress networkId =
  toPlutusType <<< UnbalancedTransaction.pubKeyHashRewardAddress networkId

pubKeyHashEnterpriseAddress :: NetworkId -> PubKeyHash -> Maybe Address
pubKeyHashEnterpriseAddress networkId =
  toPlutusType <<< UnbalancedTransaction.pubKeyHashEnterpriseAddress networkId

payPubKeyHashRewardAddress :: NetworkId -> PaymentPubKeyHash -> Maybe Address
payPubKeyHashRewardAddress networkId =
  toPlutusType <<< UnbalancedTransaction.payPubKeyHashRewardAddress networkId

payPubKeyHashBaseAddress
  :: NetworkId -> PaymentPubKeyHash -> StakePubKeyHash -> Maybe Address
payPubKeyHashBaseAddress networkId pkh =
  toPlutusType <<< UnbalancedTransaction.payPubKeyHashBaseAddress networkId pkh

payPubKeyHashEnterpriseAddress
  :: NetworkId -> PaymentPubKeyHash -> Maybe Address
payPubKeyHashEnterpriseAddress networkId =
  toPlutusType <<< UnbalancedTransaction.payPubKeyHashEnterpriseAddress
    networkId

stakePubKeyHashRewardAddress :: NetworkId -> StakePubKeyHash -> Maybe Address
stakePubKeyHashRewardAddress networkId =
  toPlutusType <<< UnbalancedTransaction.stakePubKeyHashRewardAddress networkId
