-- | A module for Address-related functionality and querying own wallet.
module Contract.Address
  ( getNetworkId
  , getWalletAddress
  , getWalletCollateral
  , module ExportAddress
  , module Bech32
  , module ByteArray
  , module PubKeyHash
  , module Scripts
  , module SerializationAddress
  , module Transaction
  , module UnbalancedTransaction
  , ownPaymentPubKeyHash
  , ownPubKeyHash
  , ownStakePubKeyHash
  ) where

import Address
  ( enterpriseAddressMintingPolicyHash
  , enterpriseAddressScriptHash
  , enterpriseAddressStakeValidatorHash
  , enterpriseAddressValidatorHash
  ) as ExportAddress
import Address (getNetworkId) as Address
import Contract.Monad (Contract, wrapContract)
import Data.Maybe (Maybe)
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
import Serialization.Address (Address)
import Serialization.Address -- There are a lot of helpers we have ignored here, we may want to include them.
  ( Slot(Slot)
  , BlockId(BlockId)
  , TransactionIndex(TransactionIndex)
  , CertificateIndex(CertificateIndex)
  , Pointer
  , Address
  , BaseAddress
  , ByronAddress
  , EnterpriseAddress
  , PointerAddress
  , RewardAddress
  , StakeCredential
  , ByronProtocolMagic(ByronProtocolMagic)
  , NetworkId(TestnetId, MainnetId)
  ) as SerializationAddress
import Types.Aliases (Bech32String) as Bech32
import Types.ByteArray (ByteArray) as ByteArray
import Types.PubKeyHash (PubKeyHash, PaymentPubKeyHash, StakePubKeyHash)
import Types.PubKeyHash
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  , StakePubKeyHash(StakePubKeyHash)
  , payPubKeyHashBaseAddress
  , payPubKeyHashRewardAddress
  , payPubKeyHashEnterpriseAddress
  , pubKeyHashBaseAddress
  , pubKeyHashEnterpriseAddress
  , stakePubKeyHashRewardAddress
  ) as PubKeyHash
import Types.UnbalancedTransaction
  ( PaymentPubKey(PaymentPubKey)
  , ScriptOutput(ScriptOutput)
  , payPubKeyRequiredSigner
  , payPubKeyVkey
  -- , pubKeyHash
  ) as UnbalancedTransaction
import Types.Transaction
  ( Ed25519Signature(Ed25519Signature)
  , PublicKey(PublicKey)
  , RequiredSigner(RequiredSigner)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  ) as Transaction
import Types.TransactionUnspentOutput (TransactionUnspentOutput)

-- | Get the `Address` of the browser wallet.
getWalletAddress :: forall (r :: Row Type). Contract r (Maybe Address)
getWalletAddress = wrapContract QueryM.getWalletAddress

-- | Get the collateral of the browser wallet. This collateral will vary
-- | depending on the wallet.
-- | E.g. Nami creates a hardcoded 5 Ada collateral.
getWalletCollateral
  :: forall (r :: Row Type). Contract r (Maybe TransactionUnspentOutput)
getWalletCollateral = wrapContract QueryM.getWalletCollateral

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
  :: forall (r :: Row Type). Contract r SerializationAddress.NetworkId
getNetworkId = wrapContract Address.getNetworkId
