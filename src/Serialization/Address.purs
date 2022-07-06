module Serialization.Address
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
  , addressBytes
  , addressBech32
  , addressNetworkId
  , intToNetworkId
  , keyHashCredential
  , scriptHashCredential
  , withStakeCredential
  , stakeCredentialToBytes
  , baseAddress
  , baseAddressPaymentCred
  , baseAddressDelegationCred
  , baseAddressToAddress
  , paymentKeyHashStakeKeyHashAddress
  , scriptHashStakeKeyHashAddress
  , paymentKeyHashScriptHashAddress
  , scriptHashScriptHashAddress
  , scriptAddress
  , ByronProtocolMagic(ByronProtocolMagic)
  , NetworkId(..)
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
  , paymentKeyHashEnterpriseAddress
  , scriptHashEnterpriseAddress
  , networkIdtoInt
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
  , paymentKeyHashPointerAddress
  , scriptHashPointerAddress
  , rewardAddress
  , rewardAddressPaymentCred
  , rewardAddressToAddress
  , rewardAddressBytes
  , rewardAddressBech32
  , rewardAddressFromBytes
  , rewardAddressFromBech32
  , rewardAddressNetworkId
  , rewardAddressFromAddress
  , unsafeIntToNetId
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson, encodeAeson')
import Aeson.Encode (encodeTagged)
import Control.Alt ((<|>))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Op (Op(Op))
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)
import Data.UInt as UInt
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import FromData (class FromData)
import Partial.Unsafe (unsafePartial)
import Serialization.Hash (Ed25519KeyHash, ScriptHash)
import Serialization.Types (Bip32PublicKey)
import ToData (class ToData, toData)
import Types.Aliases (Bech32String, Base58String)
import Types.BigNum (BigNum)
import Types.ByteArray (ByteArray)
import Types.CborBytes (CborBytes)
import Types.PlutusData (PlutusData(Bytes))

newtype Slot = Slot BigNum

derive instance Newtype Slot _
derive instance Generic Slot _
derive newtype instance Eq Slot
derive newtype instance Ord Slot
derive newtype instance DecodeAeson Slot
derive newtype instance EncodeAeson Slot
derive newtype instance FromData Slot
derive newtype instance ToData Slot

instance Show Slot where
  show = genericShow

-- it is an integer in ogmios
-- bytestring in plutus
-- uint32 in csl
newtype BlockId = BlockId UInt

derive newtype instance Eq BlockId
derive instance Newtype BlockId _
derive instance Generic BlockId _

instance EncodeAeson BlockId where
  encodeAeson' (BlockId id) = encodeAeson' (UInt.toNumber id)

instance Show BlockId where
  show = genericShow

newtype TransactionIndex = TransactionIndex BigNum

derive instance Eq TransactionIndex
derive instance Ord TransactionIndex
derive instance Newtype TransactionIndex _
derive instance Generic TransactionIndex _
derive newtype instance DecodeAeson TransactionIndex
derive newtype instance EncodeAeson TransactionIndex
derive newtype instance ToData TransactionIndex
derive newtype instance FromData TransactionIndex

instance Show TransactionIndex where
  show = genericShow

newtype CertificateIndex = CertificateIndex BigNum

derive instance Eq CertificateIndex
derive instance Ord CertificateIndex
derive instance Newtype CertificateIndex _
derive instance Generic CertificateIndex _
derive newtype instance DecodeAeson CertificateIndex
derive newtype instance EncodeAeson CertificateIndex
derive newtype instance ToData CertificateIndex
derive newtype instance FromData CertificateIndex

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

instance EncodeAeson Address where
  encodeAeson' = encodeAeson' <<< addressBech32

showVia
  :: forall (a :: Type) (b :: Type). Show b => String -> (a -> b) -> a -> String
showVia nm toShowable addr = "(" <> nm <> " " <> show (toShowable addr) <> ")"

instance Eq Address where
  eq = eq `on` addressBytes

instance Ord Address where
  compare = compare `on` addressBytes

-- FIX ME: https://github.com/Plutonomicon/cardano-transaction-lib/issues/193
-- Plutus uses `PlutusTx.makeIsDataIndexed ''Address [('Address,0)]` on their
-- record, I'm not sure if this will become an issue given our foreign
-- representation.
instance FromData Address where
  fromData (Bytes res) = addressFromBytes (wrap res)
  fromData _ = Nothing

instance ToData Address where
  toData addr = toData <<< unwrap <<< addressBytes $ addr

foreign import data BaseAddress :: Type

instance Show BaseAddress where
  show = showVia "BaseAddress" baseAddressToAddress

instance Eq BaseAddress where
  eq = eq `on` baseAddressToAddress

instance FromData BaseAddress where
  fromData (Bytes res) = baseAddressFromBytes (wrap res)
  fromData _ = Nothing

instance ToData BaseAddress where
  toData = toData <<< baseAddressToAddress

foreign import data ByronAddress :: Type

instance Eq ByronAddress where
  eq = eq `on` byronAddressToAddress

instance Show ByronAddress where
  show = showVia "ByronAddress" byronAddressToAddress

instance FromData ByronAddress where
  fromData (Bytes res) = byronAddressFromBytes (wrap res)
  fromData _ = Nothing

instance ToData ByronAddress where
  toData = toData <<< byronAddressToAddress

foreign import data EnterpriseAddress :: Type

instance Eq EnterpriseAddress where
  eq = eq `on` enterpriseAddressToAddress

instance Show EnterpriseAddress where
  show = showVia "EnterpriseAddress" enterpriseAddressToAddress

instance FromData EnterpriseAddress where
  fromData (Bytes res) = enterpriseAddressFromBytes (wrap res)
  fromData _ = Nothing

instance ToData EnterpriseAddress where
  toData = toData <<< enterpriseAddressToAddress

foreign import data PointerAddress :: Type

instance Eq PointerAddress where
  eq = eq `on` pointerAddressToAddress

instance Show PointerAddress where
  show = showVia "PointerAddress" pointerAddressToAddress

instance FromData PointerAddress where
  fromData (Bytes res) = pointerAddressFromBytes (wrap res)
  fromData _ = Nothing

instance ToData PointerAddress where
  toData = toData <<< pointerAddressToAddress

foreign import data RewardAddress :: Type

instance Eq RewardAddress where
  eq = eq `on` rewardAddressToAddress

instance Show RewardAddress where
  show = showVia "RewardAddress" rewardAddressToAddress

instance Ord RewardAddress where
  compare = compare `on` rewardAddressBytes

instance FromData RewardAddress where
  fromData (Bytes res) = rewardAddressFromBytes (wrap res)
  fromData _ = Nothing

instance ToData RewardAddress where
  toData = toData <<< rewardAddressBytes

instance EncodeAeson RewardAddress where
  encodeAeson' = encodeAeson' <<< rewardAddressBech32

foreign import data StakeCredential :: Type

instance Eq StakeCredential where
  eq = eq `on` stakeCredentialToBytes

instance Ord StakeCredential where
  compare = compare `on` stakeCredentialToBytes

instance Show StakeCredential where
  show = showVia "StakeCredenetial" $ withStakeCredential
    { onKeyHash: show, onScriptHash: show }

instance FromData StakeCredential where
  fromData (Bytes res) = stakeCredentialFromBytes $ wrap res
  fromData _ = Nothing

instance ToData StakeCredential where
  toData = toData <<< unwrap <<< stakeCredentialToBytes

instance EncodeAeson StakeCredential where
  encodeAeson' = withStakeCredential
    { onKeyHash: encodeAeson', onScriptHash: encodeAeson' }

foreign import _addressFromBech32
  :: MaybeFfiHelper -> Bech32String -> Maybe Address

foreign import _addressFromBytes :: MaybeFfiHelper -> CborBytes -> Maybe Address
foreign import addressBytes :: Address -> CborBytes
foreign import addressBech32 :: Address -> Bech32String

foreign import _addressNetworkId :: (Int -> NetworkId) -> Address -> NetworkId

addressNetworkId :: Address -> NetworkId
addressNetworkId = _addressNetworkId unsafeIntToNetId

intToNetworkId :: Int -> Maybe NetworkId
intToNetworkId = case _ of
  0 -> Just TestnetId
  1 -> Just MainnetId
  _ -> Nothing

foreign import keyHashCredential :: Ed25519KeyHash -> StakeCredential
foreign import scriptHashCredential :: ScriptHash -> StakeCredential
foreign import withStakeCredential
  :: forall (a :: Type)
   . { onKeyHash :: Ed25519KeyHash -> a, onScriptHash :: ScriptHash -> a }
  -> StakeCredential
  -> a

foreign import stakeCredentialToBytes :: StakeCredential -> CborBytes
foreign import _stakeCredentialFromBytes
  :: MaybeFfiHelper -> CborBytes -> Maybe StakeCredential

foreign import _baseAddress
  :: (NetworkId -> Int)
  -> { network :: NetworkId
     , paymentCred :: StakeCredential
     , delegationCred :: StakeCredential
     }
  -> BaseAddress

baseAddress
  :: { network :: NetworkId
     , paymentCred :: StakeCredential
     , delegationCred :: StakeCredential
     }
  -> BaseAddress
baseAddress = _baseAddress networkIdtoInt

foreign import baseAddressPaymentCred :: BaseAddress -> StakeCredential
foreign import baseAddressDelegationCred :: BaseAddress -> StakeCredential
foreign import _baseAddressFromAddress
  :: MaybeFfiHelper -> Address -> Maybe BaseAddress

foreign import baseAddressToAddress :: BaseAddress -> Address

newtype ByronProtocolMagic = ByronProtocolMagic UInt

data NetworkId
  = TestnetId
  | MainnetId

instance EncodeAeson NetworkId where
  encodeAeson' = case _ of
    TestnetId -> encodeAeson' $ encodeTagged "TestnetId" {} (Op encodeAeson)
    MainnetId -> encodeAeson' $ encodeTagged "MainnetId" {} (Op encodeAeson)

networkIdtoInt :: NetworkId -> Int
networkIdtoInt = case _ of
  TestnetId -> 0
  MainnetId -> 1

derive instance Eq NetworkId
derive instance Generic NetworkId _

instance Show NetworkId where
  show = genericShow

paymentKeyHashStakeKeyHashAddress
  :: NetworkId -> Ed25519KeyHash -> Ed25519KeyHash -> BaseAddress
paymentKeyHashStakeKeyHashAddress networkId pkh skh = baseAddress
  { network: networkId
  , paymentCred: keyHashCredential pkh
  , delegationCred: keyHashCredential skh
  }

scriptHashStakeKeyHashAddress
  :: NetworkId -> ScriptHash -> Ed25519KeyHash -> BaseAddress
scriptHashStakeKeyHashAddress networkId sh skh = baseAddress
  { network: networkId
  , paymentCred: scriptHashCredential sh
  , delegationCred: keyHashCredential skh
  }

paymentKeyHashScriptHashAddress
  :: NetworkId -> Ed25519KeyHash -> ScriptHash -> BaseAddress
paymentKeyHashScriptHashAddress networkId pkh sh = baseAddress
  { network: networkId
  , paymentCred: keyHashCredential pkh
  , delegationCred: scriptHashCredential sh
  }

scriptHashScriptHashAddress
  :: NetworkId -> ScriptHash -> ScriptHash -> BaseAddress
scriptHashScriptHashAddress networkId sh sh' = baseAddress
  { network: networkId
  , paymentCred: scriptHashCredential sh
  , delegationCred: scriptHashCredential sh'
  }

scriptAddress :: NetworkId -> ScriptHash -> BaseAddress
scriptAddress networkId sh =
  scriptHashScriptHashAddress networkId sh sh

stakeCredentialToKeyHash :: StakeCredential -> Maybe Ed25519KeyHash
stakeCredentialToKeyHash = withStakeCredential
  { onKeyHash: Just
  , onScriptHash: const Nothing
  }

stakeCredentialToScriptHash :: StakeCredential -> Maybe ScriptHash
stakeCredentialToScriptHash = withStakeCredential
  { onKeyHash: const Nothing
  , onScriptHash: Just
  }

stakeCredentialFromBytes :: CborBytes -> Maybe StakeCredential
stakeCredentialFromBytes = _stakeCredentialFromBytes maybeFfiHelper

addressFromBytes :: CborBytes -> Maybe Address
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

baseAddressBytes :: BaseAddress -> CborBytes
baseAddressBytes = baseAddressToAddress >>> addressBytes

baseAddressBech32 :: BaseAddress -> Bech32String
baseAddressBech32 = baseAddressToAddress >>> addressBech32

baseAddressFromBytes :: CborBytes -> Maybe BaseAddress
baseAddressFromBytes = addressFromBytes >=> baseAddressFromAddress

baseAddressFromBech32 :: Bech32String -> Maybe BaseAddress
baseAddressFromBech32 = addressFromBech32 >=> baseAddressFromAddress

baseAddressNetworkId :: BaseAddress -> NetworkId
baseAddressNetworkId = baseAddressToAddress >>> addressNetworkId

foreign import byronAddressToBase58 :: ByronAddress -> Base58String
foreign import _byronAddressFromBase58
  :: MaybeFfiHelper -> Base58String -> Maybe ByronAddress

byronAddressFromBase58 :: Base58String -> Maybe ByronAddress
byronAddressFromBase58 = _byronAddressFromBase58 maybeFfiHelper

foreign import _byronAddressFromBytes
  :: MaybeFfiHelper -> CborBytes -> Maybe ByronAddress

byronAddressFromBytes :: CborBytes -> Maybe ByronAddress
byronAddressFromBytes = _byronAddressFromBytes maybeFfiHelper

foreign import byronAddressBytes :: ByronAddress -> CborBytes

foreign import byronProtocolMagic :: ByronAddress -> ByronProtocolMagic
foreign import byronAddressAttributes :: ByronAddress -> ByteArray
foreign import _byronAddressNetworkId
  :: (Int -> NetworkId) -> ByronAddress -> NetworkId

byronAddressNetworkId :: ByronAddress -> NetworkId
byronAddressNetworkId = _byronAddressNetworkId unsafeIntToNetId

byronAddressFromAddress :: Address -> Maybe ByronAddress
byronAddressFromAddress = _byronAddressFromAddress maybeFfiHelper

foreign import _byronAddressFromAddress
  :: MaybeFfiHelper -> Address -> Maybe ByronAddress

foreign import byronAddressToAddress :: ByronAddress -> Address

foreign import byronAddressIsValid :: String -> Boolean

foreign import icarusFromKey
  :: Bip32PublicKey -> ByronProtocolMagic -> ByronAddress

foreign import _enterpriseAddress
  :: (NetworkId -> Int)
  -> { network :: NetworkId, paymentCred :: StakeCredential }
  -> EnterpriseAddress

enterpriseAddress
  :: { network :: NetworkId, paymentCred :: StakeCredential }
  -> EnterpriseAddress
enterpriseAddress = _enterpriseAddress networkIdtoInt

paymentKeyHashEnterpriseAddress
  :: NetworkId -> Ed25519KeyHash -> EnterpriseAddress
paymentKeyHashEnterpriseAddress networkId pkh = enterpriseAddress
  { network: networkId
  , paymentCred: keyHashCredential pkh
  }

scriptHashEnterpriseAddress
  :: NetworkId -> ScriptHash -> EnterpriseAddress
scriptHashEnterpriseAddress networkId sh = enterpriseAddress
  { network: networkId
  , paymentCred: scriptHashCredential sh
  }

foreign import enterpriseAddressPaymentCred
  :: EnterpriseAddress -> StakeCredential

foreign import _enterpriseAddressFromAddress
  :: MaybeFfiHelper -> Address -> Maybe EnterpriseAddress

foreign import enterpriseAddressToAddress :: EnterpriseAddress -> Address

enterpriseAddressFromAddress :: Address -> Maybe EnterpriseAddress
enterpriseAddressFromAddress = _enterpriseAddressFromAddress maybeFfiHelper

enterpriseAddressBytes :: EnterpriseAddress -> CborBytes
enterpriseAddressBytes = enterpriseAddressToAddress >>> addressBytes

enterpriseAddressBech32 :: EnterpriseAddress -> Bech32String
enterpriseAddressBech32 = enterpriseAddressToAddress >>> addressBech32

enterpriseAddressFromBytes :: CborBytes -> Maybe EnterpriseAddress
enterpriseAddressFromBytes = addressFromBytes >=> enterpriseAddressFromAddress

enterpriseAddressFromBech32 :: Bech32String -> Maybe EnterpriseAddress
enterpriseAddressFromBech32 = addressFromBech32 >=> enterpriseAddressFromAddress

enterpriseAddressNetworkId :: EnterpriseAddress -> NetworkId
enterpriseAddressNetworkId = enterpriseAddressToAddress >>> addressNetworkId

foreign import _pointerAddress
  :: (NetworkId -> Int)
  -> { network :: NetworkId
     , paymentCred :: StakeCredential
     , stakePointer :: Pointer
     }
  -> PointerAddress

pointerAddress
  :: { network :: NetworkId
     , paymentCred :: StakeCredential
     , stakePointer :: Pointer
     }
  -> PointerAddress
pointerAddress = _pointerAddress networkIdtoInt

paymentKeyHashPointerAddress
  :: NetworkId -> Ed25519KeyHash -> Pointer -> PointerAddress
paymentKeyHashPointerAddress networkId pkh ptr = pointerAddress
  { network: networkId
  , paymentCred: keyHashCredential pkh
  , stakePointer: ptr
  }

scriptHashPointerAddress
  :: NetworkId -> ScriptHash -> Pointer -> PointerAddress
scriptHashPointerAddress networkId sh ptr = pointerAddress
  { network: networkId
  , paymentCred: scriptHashCredential sh
  , stakePointer: ptr
  }

foreign import pointerAddressPaymentCred :: PointerAddress -> StakeCredential
foreign import _pointerAddressFromAddress
  :: MaybeFfiHelper -> Address -> Maybe PointerAddress

foreign import pointerAddressToAddress :: PointerAddress -> Address

pointerAddressFromAddress :: Address -> Maybe PointerAddress
pointerAddressFromAddress = _pointerAddressFromAddress maybeFfiHelper

foreign import pointerAddressStakePointer :: PointerAddress -> Pointer

pointerAddressBytes :: PointerAddress -> CborBytes
pointerAddressBytes = pointerAddressToAddress >>> addressBytes

pointerAddressBech32 :: PointerAddress -> Bech32String
pointerAddressBech32 = pointerAddressToAddress >>> addressBech32

pointerAddressFromBytes :: CborBytes -> Maybe PointerAddress
pointerAddressFromBytes = addressFromBytes >=> pointerAddressFromAddress

pointerAddressFromBech32 :: Bech32String -> Maybe PointerAddress
pointerAddressFromBech32 = addressFromBech32 >=> pointerAddressFromAddress

pointerAddressNetworkId :: PointerAddress -> NetworkId
pointerAddressNetworkId = pointerAddressToAddress >>> addressNetworkId

foreign import _rewardAddress
  :: (NetworkId -> Int)
  -> { network :: NetworkId, paymentCred :: StakeCredential }
  -> RewardAddress

rewardAddress
  :: { network :: NetworkId, paymentCred :: StakeCredential } -> RewardAddress
rewardAddress = _rewardAddress networkIdtoInt

foreign import rewardAddressPaymentCred :: RewardAddress -> StakeCredential
foreign import _rewardAddressFromAddress
  :: MaybeFfiHelper -> Address -> Maybe RewardAddress

foreign import rewardAddressToAddress :: RewardAddress -> Address

rewardAddressFromAddress :: Address -> Maybe RewardAddress
rewardAddressFromAddress = _rewardAddressFromAddress maybeFfiHelper

rewardAddressBytes :: RewardAddress -> CborBytes
rewardAddressBytes = rewardAddressToAddress >>> addressBytes

rewardAddressBech32 :: RewardAddress -> Bech32String
rewardAddressBech32 = rewardAddressToAddress >>> addressBech32

rewardAddressFromBytes :: CborBytes -> Maybe RewardAddress
rewardAddressFromBytes = addressFromBytes >=> rewardAddressFromAddress

rewardAddressFromBech32 :: Bech32String -> Maybe RewardAddress
rewardAddressFromBech32 = addressFromBech32 >=> rewardAddressFromAddress

rewardAddressNetworkId :: RewardAddress -> NetworkId
rewardAddressNetworkId = rewardAddressToAddress >>> addressNetworkId

-- based on the assumption that CSL will never return invalid networkid
unsafeIntToNetId :: Int -> NetworkId
unsafeIntToNetId i = unsafePartial $ fromJust $ intToNetworkId i
