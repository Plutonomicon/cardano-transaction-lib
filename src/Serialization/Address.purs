module Serialization.Address
  ( Slot(Slot)
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
  , addressBytes
  , addressBech32
  , addressNetworkId
  , keyHashCredential
  , scriptHashCredential
  , withStakeCredential
  , stakeCredentialToBytes
  , baseAddress
  , baseAddressPaymentCred
  , baseAddressDelegationCred
  , baseAddressToAddress
  , ByronProtocolMagic(ByronProtocolMagic)
  , NetworkId(NetworkId)
  , testnetId
  , mainnetId
  , pubKeyAddress
  , scriptAddress
  , stakeCredentialToKeyHash
  , stakeCredentialToScriptHash
  , stakeCredentialFromBytes
  , addressFromBytes
  , addressFromBech32
  , addressPaymentCred
  , baseAddressFromAddress
  , baseAddressBytes
  , baseAddressBech32
  , baseAddressFromBytes
  , baseAddressFromBech32
  , baseAddressNetworkId
  , byronAddressToBase58
  , byronAddressFromBase58
  , byronAddressFromBytes
  , byronAddressBytes
  , byronProtocolMagic
  , byronAddressAttributes
  , byronAddressNetworkId
  , byronAddressFromAddress
  , byronAddressToAddress
  , byronAddressIsValid
  , icarusFromKey
  , enterpriseAddress
  , enterpriseAddressPaymentCred
  , enterpriseAddressToAddress
  , enterpriseAddressFromAddress
  , enterpriseAddressBytes
  , enterpriseAddressBech32
  , enterpriseAddressFromBytes
  , enterpriseAddressFromBech32
  , enterpriseAddressNetworkId
  , pointerAddress
  , pointerAddressPaymentCred
  , pointerAddressToAddress
  , pointerAddressFromAddress
  , pointerAddressStakePointer
  , pointerAddressBytes
  , pointerAddressBech32
  , pointerAddressFromBytes
  , pointerAddressFromBech32
  , pointerAddressNetworkId
  , rewardAddress
  , rewardAddressPaymentCred
  , rewardAddressToAddress
  , rewardAddressBytes
  , rewardAddressBech32
  , rewardAddressFromBytes
  , rewardAddressFromBech32
  , rewardAddressNetworkId
  , rewardAddressFromAddress
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.BigInt (BigInt)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization.Hash (Ed25519KeyHash, ScriptHash)
import Serialization.Types (Bip32PublicKey)
import Types.Aliases (Bech32String, Base58String)
import Types.ByteArray (ByteArray)

-- Is UInt going to be an issue? For example, see beginningOfTime and the fact
-- we're limited to 4294967295 slots with UInt. I've changed it back to BigInt,
-- any objections welcome.
newtype Slot = Slot UInt

derive newtype instance Eq Slot
derive newtype instance Ord Slot
derive instance Newtype Slot _
derive instance Generic Slot _

instance Show Slot where
  show = genericShow

instance Semigroup Slot where
  append (Slot s1) (Slot s2) = Slot $ s1 + s2

instance Monoid Slot where
  mempty = Slot zero

newtype TransactionIndex = TransactionIndex UInt

derive instance Eq TransactionIndex
derive instance Newtype TransactionIndex _
derive instance Generic TransactionIndex _

instance Show TransactionIndex where
  show = genericShow

newtype CertificateIndex = CertificateIndex UInt

derive instance Eq CertificateIndex
derive instance Newtype CertificateIndex _
derive instance Generic CertificateIndex _

instance Show CertificateIndex where
  show = genericShow

type Pointer =
  { slot :: Slot
  , txIx :: TransactionIndex
  , certIx :: CertificateIndex
  }

foreign import data Address :: Type

instance Show Address where
  show a = "(Address " <> addressBech32 a <> ")"

showVia :: forall (a :: Type) (b :: Type). Show b => String -> (a -> b) -> a -> String
showVia nm toShowable addr = "(" <> nm <> " " <> show (toShowable addr) <> ")"

instance Eq Address where
  eq = eq `on` addressBytes

foreign import data BaseAddress :: Type

instance Show BaseAddress where
  show = showVia "BaseAddress" baseAddressToAddress

instance Eq BaseAddress where
  eq = eq `on` baseAddressToAddress

foreign import data ByronAddress :: Type

instance Eq ByronAddress where
  eq = eq `on` byronAddressToAddress

instance Show ByronAddress where
  show = showVia "ByronAddress" byronAddressToAddress

foreign import data EnterpriseAddress :: Type

instance Eq EnterpriseAddress where
  eq = eq `on` enterpriseAddressToAddress

instance Show EnterpriseAddress where
  show = showVia "EnterpriseAddress" enterpriseAddressToAddress

foreign import data PointerAddress :: Type

instance Eq PointerAddress where
  eq = eq `on` pointerAddressToAddress

instance Show PointerAddress where
  show = showVia "PointerAddress" pointerAddressToAddress

foreign import data RewardAddress :: Type

instance Eq RewardAddress where
  eq = eq `on` rewardAddressToAddress

instance Show RewardAddress where
  show = showVia "RewardAddress" rewardAddressToAddress

instance Ord RewardAddress where
  compare = compare `on` rewardAddressBytes

foreign import data StakeCredential :: Type

instance Eq StakeCredential where
  eq = eq `on` stakeCredentialToBytes

instance Show StakeCredential where
  show = showVia "StakeCredenetial" $ withStakeCredential
    { onKeyHash: show, onScriptHash: show }

foreign import _addressFromBech32 :: MaybeFfiHelper -> Bech32String -> Maybe Address
foreign import _addressFromBytes :: MaybeFfiHelper -> ByteArray -> Maybe Address
foreign import addressBytes :: Address -> ByteArray
foreign import addressBech32 :: Address -> Bech32String
foreign import addressNetworkId :: Address -> NetworkId

foreign import keyHashCredential :: Ed25519KeyHash -> StakeCredential
foreign import scriptHashCredential :: ScriptHash -> StakeCredential
foreign import withStakeCredential :: forall (a :: Type). { onKeyHash :: Ed25519KeyHash -> a, onScriptHash :: ScriptHash -> a } -> StakeCredential -> a
foreign import stakeCredentialToBytes :: StakeCredential -> ByteArray
foreign import _stakeCredentialFromBytes :: MaybeFfiHelper -> ByteArray -> Maybe StakeCredential

foreign import baseAddress
  :: { network :: NetworkId
     , paymentCred :: StakeCredential
     , delegationCred :: StakeCredential
     }
  -> BaseAddress

foreign import baseAddressPaymentCred :: BaseAddress -> StakeCredential
foreign import baseAddressDelegationCred :: BaseAddress -> StakeCredential
foreign import _baseAddressFromAddress :: MaybeFfiHelper -> Address -> Maybe BaseAddress
foreign import baseAddressToAddress :: BaseAddress -> Address

newtype ByronProtocolMagic = ByronProtocolMagic UInt

newtype NetworkId = NetworkId Int

derive instance Eq NetworkId
derive instance Generic NetworkId _

instance Show NetworkId where
  show = genericShow

testnetId :: NetworkId
testnetId = NetworkId 0

mainnetId :: NetworkId
mainnetId = NetworkId 1

pubKeyAddress :: NetworkId -> Ed25519KeyHash -> BaseAddress
pubKeyAddress netId pkh = baseAddress { network: netId, paymentCred: keyHashCredential pkh, delegationCred: keyHashCredential pkh }

scriptAddress :: NetworkId -> ScriptHash -> BaseAddress
scriptAddress netId skh = baseAddress { network: netId, paymentCred: scriptHashCredential skh, delegationCred: scriptHashCredential skh }

stakeCredentialToKeyHash :: StakeCredential -> Maybe Ed25519KeyHash
stakeCredentialToKeyHash = withStakeCredential { onKeyHash: Just, onScriptHash: const Nothing }

stakeCredentialToScriptHash :: StakeCredential -> Maybe ScriptHash
stakeCredentialToScriptHash = withStakeCredential { onKeyHash: const Nothing, onScriptHash: Just }

stakeCredentialFromBytes :: ByteArray -> Maybe StakeCredential
stakeCredentialFromBytes = _stakeCredentialFromBytes maybeFfiHelper

addressFromBytes :: ByteArray -> Maybe Address
addressFromBytes = _addressFromBytes maybeFfiHelper

addressFromBech32 :: Bech32String -> Maybe Address
addressFromBech32 = _addressFromBech32 maybeFfiHelper

addressPaymentCred :: Address -> Maybe StakeCredential
addressPaymentCred addr =
  (baseAddressPaymentCred <$> baseAddressFromAddress addr)
    <|> (rewardAddressPaymentCred <$> rewardAddressFromAddress addr)
    <|> (pointerAddressPaymentCred <$> pointerAddressFromAddress addr)
    <|>
      (enterpriseAddressPaymentCred <$> enterpriseAddressFromAddress addr)

baseAddressFromAddress :: Address -> Maybe BaseAddress
baseAddressFromAddress = _baseAddressFromAddress maybeFfiHelper

baseAddressBytes :: BaseAddress -> ByteArray
baseAddressBytes = baseAddressToAddress >>> addressBytes

baseAddressBech32 :: BaseAddress -> Bech32String
baseAddressBech32 = baseAddressToAddress >>> addressBech32

baseAddressFromBytes :: ByteArray -> Maybe BaseAddress
baseAddressFromBytes = addressFromBytes >=> baseAddressFromAddress

baseAddressFromBech32 :: Bech32String -> Maybe BaseAddress
baseAddressFromBech32 = addressFromBech32 >=> baseAddressFromAddress

baseAddressNetworkId :: BaseAddress -> NetworkId
baseAddressNetworkId = baseAddressToAddress >>> addressNetworkId

foreign import byronAddressToBase58 :: ByronAddress -> Base58String
foreign import _byronAddressFromBase58 :: MaybeFfiHelper -> Base58String -> Maybe ByronAddress

byronAddressFromBase58 :: Base58String -> Maybe ByronAddress
byronAddressFromBase58 = _byronAddressFromBase58 maybeFfiHelper

foreign import _byronAddressFromBytes :: MaybeFfiHelper -> ByteArray -> Maybe ByronAddress

byronAddressFromBytes :: ByteArray -> Maybe ByronAddress
byronAddressFromBytes = _byronAddressFromBytes maybeFfiHelper

foreign import byronAddressBytes :: ByronAddress -> ByteArray

foreign import byronProtocolMagic :: ByronAddress -> ByronProtocolMagic
foreign import byronAddressAttributes :: ByronAddress -> ByteArray
foreign import byronAddressNetworkId :: ByronAddress -> NetworkId

byronAddressFromAddress :: Address -> Maybe ByronAddress
byronAddressFromAddress = _byronAddressFromAddress maybeFfiHelper

foreign import _byronAddressFromAddress :: MaybeFfiHelper -> Address -> Maybe ByronAddress
foreign import byronAddressToAddress :: ByronAddress -> Address

foreign import byronAddressIsValid :: String -> Boolean

foreign import icarusFromKey :: Bip32PublicKey -> ByronProtocolMagic -> ByronAddress

foreign import enterpriseAddress :: { network :: NetworkId, paymentCred :: StakeCredential } -> EnterpriseAddress
foreign import enterpriseAddressPaymentCred :: EnterpriseAddress -> StakeCredential
foreign import _enterpriseAddressFromAddress :: MaybeFfiHelper -> Address -> Maybe EnterpriseAddress
foreign import enterpriseAddressToAddress :: EnterpriseAddress -> Address

enterpriseAddressFromAddress :: Address -> Maybe EnterpriseAddress
enterpriseAddressFromAddress = _enterpriseAddressFromAddress maybeFfiHelper

enterpriseAddressBytes :: EnterpriseAddress -> ByteArray
enterpriseAddressBytes = enterpriseAddressToAddress >>> addressBytes

enterpriseAddressBech32 :: EnterpriseAddress -> Bech32String
enterpriseAddressBech32 = enterpriseAddressToAddress >>> addressBech32

enterpriseAddressFromBytes :: ByteArray -> Maybe EnterpriseAddress
enterpriseAddressFromBytes = addressFromBytes >=> enterpriseAddressFromAddress

enterpriseAddressFromBech32 :: Bech32String -> Maybe EnterpriseAddress
enterpriseAddressFromBech32 = addressFromBech32 >=> enterpriseAddressFromAddress

enterpriseAddressNetworkId :: EnterpriseAddress -> NetworkId
enterpriseAddressNetworkId = enterpriseAddressToAddress >>> addressNetworkId

foreign import pointerAddress :: { network :: NetworkId, paymentCred :: StakeCredential, stakePointer :: Pointer } -> PointerAddress
foreign import pointerAddressPaymentCred :: PointerAddress -> StakeCredential
foreign import _pointerAddressFromAddress :: MaybeFfiHelper -> Address -> Maybe PointerAddress
foreign import pointerAddressToAddress :: PointerAddress -> Address

pointerAddressFromAddress :: Address -> Maybe PointerAddress
pointerAddressFromAddress = _pointerAddressFromAddress maybeFfiHelper

foreign import pointerAddressStakePointer :: PointerAddress -> Pointer

pointerAddressBytes :: PointerAddress -> ByteArray
pointerAddressBytes = pointerAddressToAddress >>> addressBytes

pointerAddressBech32 :: PointerAddress -> Bech32String
pointerAddressBech32 = pointerAddressToAddress >>> addressBech32

pointerAddressFromBytes :: ByteArray -> Maybe PointerAddress
pointerAddressFromBytes = addressFromBytes >=> pointerAddressFromAddress

pointerAddressFromBech32 :: Bech32String -> Maybe PointerAddress
pointerAddressFromBech32 = addressFromBech32 >=> pointerAddressFromAddress

pointerAddressNetworkId :: PointerAddress -> NetworkId
pointerAddressNetworkId = pointerAddressToAddress >>> addressNetworkId

foreign import rewardAddress :: { network :: NetworkId, paymentCred :: StakeCredential } -> RewardAddress
foreign import rewardAddressPaymentCred :: RewardAddress -> StakeCredential
foreign import _rewardAddressFromAddress :: MaybeFfiHelper -> Address -> Maybe RewardAddress
foreign import rewardAddressToAddress :: RewardAddress -> Address

rewardAddressFromAddress :: Address -> Maybe RewardAddress
rewardAddressFromAddress = _rewardAddressFromAddress maybeFfiHelper

rewardAddressBytes :: RewardAddress -> ByteArray
rewardAddressBytes = rewardAddressToAddress >>> addressBytes

rewardAddressBech32 :: RewardAddress -> Bech32String
rewardAddressBech32 = rewardAddressToAddress >>> addressBech32

rewardAddressFromBytes :: ByteArray -> Maybe RewardAddress
rewardAddressFromBytes = addressFromBytes >=> rewardAddressFromAddress

rewardAddressFromBech32 :: Bech32String -> Maybe RewardAddress
rewardAddressFromBech32 = addressFromBech32 >=> rewardAddressFromAddress

rewardAddressNetworkId :: RewardAddress -> NetworkId
rewardAddressNetworkId = rewardAddressToAddress >>> addressNetworkId
