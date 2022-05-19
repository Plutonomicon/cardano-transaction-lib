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

import Prelude

import Address
  ( enterpriseAddressMintingPolicyHash
  , enterpriseAddressScriptHash
  , enterpriseAddressStakeValidatorHash
  , enterpriseAddressValidatorHash
  ) as ExportAddress
import Address (getNetworkId) as Address
import Cardano.Types.Transaction
  ( Ed25519Signature(Ed25519Signature)
  , PublicKey(PublicKey)
  , RequiredSigner(RequiredSigner)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  ) as Transaction
import Contract.Monad (Contract, wrapContract, liftedM)
import Data.Maybe (Maybe)
import Data.Traversable (for)
import Plutus.ToPlutusType (toPlutusType)
import Plutus.Types.Address (Address)
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
import Types.Aliases (Bech32String) as Bech32
import Types.ByteArray (ByteArray) as ByteArray
import Types.PubKeyHash (PubKeyHash)
import Types.PubKeyHash (PubKeyHash(PubKeyHash)) as PubKeyHash
import Types.UnbalancedTransaction (StakePubKeyHash, PaymentPubKeyHash)
import Types.UnbalancedTransaction
  ( PaymentPubKey(PaymentPubKey)
  , PaymentPubKeyHash(PaymentPubKeyHash)
  , ScriptOutput(ScriptOutput)
  , StakePubKeyHash(StakePubKeyHash)
  , payPubKeyHashBaseAddress
  , payPubKeyHashRewardAddress
  , payPubKeyHashEnterpriseAddress
  , payPubKeyRequiredSigner
  , payPubKeyVkey
  -- , pubKeyHash
  , pubKeyHashBaseAddress
  , pubKeyHashEnterpriseAddress
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
  :: forall (r :: Row Type). Contract r SerializationAddress.NetworkId
getNetworkId = wrapContract Address.getNetworkId
