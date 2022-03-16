module Contract.Address
  ( getWalletAddress
  , getWalletCollateral
  , ownPaymentPubKeyHash
  , ownPubKeyHash
  , module Address
  , module ByteArray
  , module Bech32
  , module Transaction
  , module UnbalancedTransaction
  ) where

import Prelude

import Contract.Monad (Contract)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import QueryM (getWalletAddress, getWalletCollateral) as QueryM
import Serialization.Address (Address, addressBech32)
import Serialization.Address -- Do we want to export all the helpers from Address?
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
  -- , addressBytes
  -- , addressBech32
  -- , addressNetworkId
  -- , intToNetworkId
  -- , keyHashCredential
  -- , scriptHashCredential
  -- , withStakeCredential
  -- , stakeCredentialToBytes
  -- , baseAddress
  -- , baseAddressPaymentCred
  -- , baseAddressDelegationCred
  -- , baseAddressToAddress
  , ByronProtocolMagic(ByronProtocolMagic)
  , NetworkId(TestnetId, MainnetId)
  -- , pubKeyAddress
  -- , scriptAddress
  -- , stakeCredentialToKeyHash
  -- , stakeCredentialToScriptHash
  -- , stakeCredentialFromBytes
  -- , addressFromBytes
  -- , addressFromBech32
  -- , addressPaymentCred
  -- , baseAddressFromAddress
  -- , baseAddressBytes
  -- , baseAddressBech32
  -- , baseAddressFromBytes
  -- , baseAddressFromBech32
  -- , baseAddressNetworkId
  -- , byronAddressToBase58
  -- , byronAddressFromBase58
  -- , byronAddressFromBytes
  -- , byronAddressBytes
  -- , byronProtocolMagic
  -- , byronAddressAttributes
  -- , byronAddressNetworkId
  -- , byronAddressFromAddress
  -- , byronAddressToAddress
  -- , byronAddressIsValid
  -- , icarusFromKey
  -- , enterpriseAddress
  -- , enterpriseAddressPaymentCred
  -- , enterpriseAddressToAddress
  -- , enterpriseAddressFromAddress
  -- , enterpriseAddressBytes
  -- , enterpriseAddressBech32
  -- , enterpriseAddressFromBytes
  -- , enterpriseAddressFromBech32
  -- , enterpriseAddressNetworkId
  -- , networkIdtoInt
  -- , pointerAddress
  -- , pointerAddressPaymentCred
  -- , pointerAddressToAddress
  -- , pointerAddressFromAddress
  -- , pointerAddressStakePointer
  -- , pointerAddressBytes
  -- , pointerAddressBech32
  -- , pointerAddressFromBytes
  -- , pointerAddressFromBech32
  -- , pointerAddressNetworkId
  -- , rewardAddress
  -- , rewardAddressPaymentCred
  -- , rewardAddressToAddress
  -- , rewardAddressBytes
  -- , rewardAddressBech32
  -- , rewardAddressFromBytes
  -- , rewardAddressFromBech32
  -- , rewardAddressNetworkId
  -- , rewardAddressFromAddress
  ) as Address
import Types.Aliases (Bech32String) as Bech32
import Types.ByteArray (ByteArray) as ByteArray
import Types.UnbalancedTransaction
  ( PubKeyHash
  , PaymentPubKeyHash
  , pubKeyHash
  )
import Types.UnbalancedTransaction
  ( PaymentPubKey(PaymentPubKey)
  , PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  , ScriptOutput(ScriptOutput)
  , StakeKeyHash(StakeKeyHash)
  , StakePubKeyHash(StakePubKeyHash)
  -- , TxOutRef
  -- , UnbalancedTx(..)
  -- , _transaction
  -- , _utxoIndex
  -- , emptyUnbalancedTx
  , payPubKeyHash
  , payPubKeyHashAddress
  , payPubKeyHashBaseAddress
  , payPubKeyRequiredSigner
  , payPubKeyVkey
  , pubKeyHash
  , pubKeyHashAddress
  , pubKeyHashBaseAddress
  -- , scriptOutputToTxOutput
  , stakeKeyHashAddress
  , stakeKeyHashBaseAddress
  , stakePubKeyHashAddress
  , stakePubKeyHashBaseAddress
  -- , utxoIndexToUtxo
  ) as UnbalancedTransaction
import Types.Transaction
  ( Ed25519Signature(Ed25519Signature)
  , PublicKey(PublicKey)
  , RequiredSigner(RequiredSigner)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  ) as Transaction
import Types.TransactionUnspentOutput (TransactionUnspentOutput)

-- | A module for Address-related functionality and querying own wallet.

-- | Get the `Address` of the browser wallet.
getWalletAddress :: Contract (Maybe Address)
getWalletAddress = wrap QueryM.getWalletAddress

-- | Get the collateral of the browser wallet. This collateral will vary
-- | depending on the wallet.
-- | E.g. Nami creates a hardcoded 5 Ada collateral.
getWalletCollateral :: Contract (Maybe TransactionUnspentOutput)
getWalletCollateral = wrap QueryM.getWalletCollateral

-- | Gets wallet `PubKeyHash` via `getWalletAddress`.
ownPubKeyHash :: Contract (Maybe PubKeyHash)
ownPubKeyHash =
  map ((=<<) pubKeyHash <<< map (wrap <<< addressBech32)) $ getWalletAddress

-- | Gets wallet `PaymentPubKeyHash` via `getWalletAddress`.
ownPaymentPubKeyHash :: Contract (Maybe PaymentPubKeyHash)
ownPaymentPubKeyHash = map wrap <$> ownPubKeyHash