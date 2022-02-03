module Serialization.Address
  ( class Address
  , AddressCsl
  , BaseAddressCsl
  , BaseAddress
  , RewardAddress
  , RewardAddressCsl
  , PointerAddressCsl
  , NetworkTag(TestnetTag, MainnetTag)
  , StakeCred
  , CredType
  , StakeCredentialCsl
  , PubKeyAddress
  , ScriptAddress
  , addressBech32
  , addressBytes
  , addrR
  , baseAddressFromBech32
  , baseAddressFromBytes
  , rewardAddressFromBytes
  , rewardAddressFromBech32
  , ed25519KeyHashCredType
  , baseAddress
  , rewardAddress
  , netTagToInt
  , pubKeyAddress
  , scriptAddress
  , scriptHashCredType
  , networkTag
  , networkTagInt
  , paymentCred
  , delegationCred
  , toAddressCsl
  , intToNetTag
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization.Csl (class ToCsl, CslType, toCslRep, toCslType)
import Serialization.Hash (Ed25519KeyHash, ScriptHash)
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray)

-- | Denotes one of two possible values of Shelley address' network tag.
data NetworkTag = TestnetTag | MainnetTag

derive instance Eq NetworkTag

instance Show NetworkTag where
  show = case _ of
    TestnetTag -> "TestnetTag"
    MainnetTag -> "MainnetTag"

-- | Simple safety wrapper to ensure that only valid types are provided
-- | as address' stake credentials.
newtype StakeCred a = StakeCred a

foreign import data StakeCredentialCsl :: Type

instance ToCsl (StakeCred Ed25519KeyHash) StakeCredentialCsl where
  toCslRep (StakeCred x) = _stakeCredFromKeyHash x

instance ToCsl (StakeCred ScriptHash) StakeCredentialCsl where
  toCslRep (StakeCred x) = _stakeCredFromScriptHash x

-- | `Address` CSL representation - a generic address type
foreign import data AddressCsl :: Type

-- | Record containing address components
type AddrR (p :: Type) (d :: Type) =
  { network :: NetworkTag
  , payment :: p
  , delegation :: d
  }

-- | Instance helper
newtype ToAddressCsl a = ToAddressCsl a

-- | All addresses should implement this class
class (ToCsl (ToAddressCsl a) AddressCsl) <= Address a (p :: Type) (d :: Type) | a -> p, a -> d where
  addrR :: a -> AddrR p d

----

-- TODO pointer address
foreign import data PointerAddressCsl :: Type

----

-- | Also known as StakeAddress
newtype RewardAddress (p :: Type) = RewardAddress (AddrR p Unit)

derive instance (Eq p) => Eq (RewardAddress p)

-- | `Address` CSL representation - a generic address type
foreign import data RewardAddressCsl :: Type

-- | instance Address RewardAddress
instance (ToCsl (StakeCred p) StakeCredentialCsl) => Address (RewardAddress p) p Unit where
  addrR (RewardAddress a) = a

-- | RewardAddress -> RewardAddressCsl
instance (ToCsl (StakeCred p) StakeCredentialCsl) => ToCsl (RewardAddress p) RewardAddressCsl where
  toCslRep x = _newRewardAddressCsl
    { network: networkTagInt x
    , paymentStakeCred: toCslRep $ StakeCred $ paymentCred x
    }

-- | RewardAddress -> AddressCsl
instance (ToCsl (StakeCred p) StakeCredentialCsl) => ToCsl (ToAddressCsl (RewardAddress p)) AddressCsl where
  toCslRep (ToAddressCsl x) = _toAddressCslUnsafe (toCslType x)

----

-- | Shelley-era address where payment and delegation parts
-- | are each either a `Ed25519Keyhash` or a `ScriptHash`
newtype BaseAddress (p :: Type) (d :: Type) = BaseAddress (AddrR p d)

derive instance (Eq p, Eq d) => Eq (BaseAddress p d)

-- | BaseAddress CSL representation
foreign import data BaseAddressCsl :: Type

-- | instance Address BaseAddress
instance (ToCsl (StakeCred p) StakeCredentialCsl, ToCsl (StakeCred d) StakeCredentialCsl) => Address (BaseAddress p d) p d where
  addrR (BaseAddress a) = a

-- | BaseAddress -> BaseAddressCsl
-- | With this instance ou can safely convert BaseAddress to csl rep
-- | to use csl functions
instance (ToCsl (StakeCred d) StakeCredentialCsl, ToCsl (StakeCred p) StakeCredentialCsl) => ToCsl (BaseAddress p d) BaseAddressCsl where
  toCslRep a = _newBaseAddressCsl
    { network: networkTagInt a
    , paymentStakeCred: toCslRep $ StakeCred $ paymentCred a
    , delegationStakeCred: toCslRep $ StakeCred $ delegationCred a
    }

-- | BaseAddress -> AddressCsl
instance (ToCsl (StakeCred p) StakeCredentialCsl, ToCsl (StakeCred d) StakeCredentialCsl) => ToCsl (ToAddressCsl (BaseAddress p d)) AddressCsl where
  toCslRep (ToAddressCsl x) = _toAddressCslUnsafe (toCslType x)

----

-- | Address where payment and stake delegation rights are bound to the same PubKeyHash
type PubKeyAddress = BaseAddress Ed25519KeyHash Ed25519KeyHash

-- | Address where payment and stake delegation rights are bound to the same ScriptHash
type ScriptAddress = BaseAddress ScriptHash ScriptHash

-- | Helper for reclaiming `BaseAddress` type arguments during decoding.
newtype CredType (p :: Type) = CredType String

----

foreign import _stakeCredFromKeyHash :: Ed25519KeyHash -> StakeCredentialCsl
foreign import _stakeCredFromScriptHash :: ScriptHash -> StakeCredentialCsl

-- | Only use on csl types that supports CSL's to_address()
foreign import _toAddressCslUnsafe :: CslType -> AddressCsl

foreign import _newRewardAddressCsl
  :: { network :: Int
     , paymentStakeCred :: StakeCredentialCsl
     }
  -> RewardAddressCsl

foreign import _newBaseAddressCsl
  :: { network :: Int
     , paymentStakeCred :: StakeCredentialCsl
     , delegationStakeCred :: StakeCredentialCsl
     }
  -> BaseAddressCsl

foreign import _addressBytesImpl :: AddressCsl -> ByteArray
foreign import _addressBech32Impl :: AddressCsl -> Bech32String

-- | Ensures that stake credentials are of requested type
-- therefore allowing to set BaseAddress' type arguments
foreign import _headerCheckBaseAddr
  :: forall p d
   . MaybeFfiHelper
  -> { payment :: CredType p
     , delegation :: CredType d
     }
  -> (Int -> Maybe NetworkTag)
  -> BaseAddressCsl
  -> Maybe (BaseAddress p d)

-- | Ensures that stake credentials are of requested type
-- therefore allowing to set RewardAddress' type arguments
foreign import _headerCheckRewardAddr
  :: forall p
   . MaybeFfiHelper
  -> { payment :: CredType p }
  -> (Int -> Maybe NetworkTag)
  -> RewardAddressCsl
  -> Maybe (RewardAddress p)

foreign import _baseAddressFromBytesImpl :: MaybeFfiHelper -> ByteArray -> Maybe BaseAddressCsl
foreign import _baseAddressFromBech32Impl :: MaybeFfiHelper -> Bech32String -> Maybe BaseAddressCsl

foreign import _rewardAddressFromBytesImpl :: MaybeFfiHelper -> ByteArray -> Maybe RewardAddressCsl
foreign import _rewardAddressFromBech32Impl :: MaybeFfiHelper -> Bech32String -> Maybe RewardAddressCsl

-- | The exported constructor for `BaseAddress`.
baseAddress
  :: forall p d
   . (ToCsl (StakeCred d) StakeCredentialCsl)
  => (ToCsl (StakeCred p) StakeCredentialCsl)
  => { network :: NetworkTag, payment :: p, delegation :: d }
  -> BaseAddress p d
baseAddress { network, payment, delegation } = BaseAddress
  { network: network
  , payment: payment
  , delegation: delegation
  }

-- | The exported constructor for `RewardAddress`.
rewardAddress
  :: forall p
   . (ToCsl (StakeCred p) StakeCredentialCsl)
  => { network :: NetworkTag, payment :: p }
  -> RewardAddress p
rewardAddress { network, payment } = RewardAddress
  { network: network
  , payment: payment
  , delegation: unit
  }

-- | Convert address to its byte representation
addressBytes
  :: forall a p d
   . Address a p d
  => a
  -> ByteArray
addressBytes = ToAddressCsl >>> toCslRep >>> _addressBytesImpl

-- | Parse `RewardAddress` from its byte representation
rewardAddressFromBytes
  :: forall p
   . { payment :: CredType p
     }
  -> ByteArray
  -> Maybe (RewardAddress p)
rewardAddressFromBytes checks bts = do
  addrCsl <- _rewardAddressFromBytesImpl maybeFfiHelper bts
  _headerCheckRewardAddr maybeFfiHelper checks intToNetTag addrCsl

-- | Parse `BaseAddress` from its byte representation
baseAddressFromBytes
  :: forall p d
   . { payment :: CredType p
     , delegation :: CredType d
     }
  -> ByteArray
  -> Maybe (BaseAddress p d)
baseAddressFromBytes checks bts = do
  addrCsl <- _baseAddressFromBytesImpl maybeFfiHelper bts
  _headerCheckBaseAddr maybeFfiHelper checks intToNetTag addrCsl

-- | Return Cardano Bech32 representation of BaseAddress
-- NOTE on safety - even if CSL functions signals that it can error out,
-- following their implementation indicates that any valid address should
-- be encoded to bech32 without erros.
addressBech32
  :: forall a p d
   . Address a p d
  => a
  -> Bech32String
addressBech32 = ToAddressCsl >>> toCslRep >>> _addressBech32Impl

-- | Build address out of its bech32 representation.
-- | For example to build `Maybe (BaseAddress Ed25519KeyHash ScriptHash)`:
-- |
-- | ```purescript
-- | >>> let addr =
-- | >>>      baseAddressFromBech32
-- | >>>       { payment: ed25519KeyHashCredType
-- | >>>       , delegation: scriptHashCredType
-- | >>>      "addr1..."
-- | '''
-- |
-- | If the string doesn't encode the address matching given credential types
-- | the function will return `Nothing`
-- |
baseAddressFromBech32 :: forall p d. { payment :: CredType p, delegation :: CredType d } -> Bech32String -> Maybe (BaseAddress p d)
baseAddressFromBech32 checks bchString = do
  addrCsl <- _baseAddressFromBech32Impl maybeFfiHelper bchString
  _headerCheckBaseAddr maybeFfiHelper checks intToNetTag addrCsl

-- | Build address out of its bech32 representation.
-- | Works analogically to `baseAddressFromBech32`.
-- | For example:
-- |
-- | ```purescript
-- | >>> let addr =
-- | >>>      rewardAddressFromBech32
-- | >>>       { payment: ed25519KeyHashCredType }
-- | >>>      "addr1..."
-- | '''
-- |
rewardAddressFromBech32 :: forall p. { payment :: CredType p } -> Bech32String -> Maybe (RewardAddress p)
rewardAddressFromBech32 checks bchString = do
  addrCsl <- _rewardAddressFromBech32Impl maybeFfiHelper bchString
  _headerCheckRewardAddr maybeFfiHelper checks intToNetTag addrCsl

-- | Build `PubKeyAddress` from a single credential.
pubKeyAddress :: NetworkTag -> Ed25519KeyHash -> PubKeyAddress
pubKeyAddress nt pkh = BaseAddress { network: nt, payment: pkh, delegation: pkh }

-- | Build `ScriptAddress` from a single credential.
scriptAddress :: NetworkTag -> ScriptHash -> ScriptAddress
scriptAddress nt sh = BaseAddress { network: nt, payment: sh, delegation: sh }

---- cred types

-- | Sets BaseAddress type arguments during decoding.
ed25519KeyHashCredType :: CredType Ed25519KeyHash
ed25519KeyHashCredType = CredType "from_keyhash"

-- | Sets BaseAddress type arguments during decoding.
scriptHashCredType :: CredType ScriptHash
scriptHashCredType = CredType "from_scripthash"

-- | NetworkTag accessor on an Address
networkTag
  :: forall a p d
   . Address a p d
  => a
  -> NetworkTag
networkTag = addrR >>> _.network

-- | NetworkTag accessor on an Address
networkTagInt
  :: forall a p d
   . Address a p d
  => a
  -> Int
networkTagInt = addrR >>> _.network >>> netTagToInt

-- | Payment credential accessor on an Address
paymentCred
  :: forall a p x
   . Address a p x
  => a
  -> p
paymentCred = addrR >>> _.payment

-- | Delegation credential accessor on an Address
delegationCred
  :: forall a x d
   . Address a x d
  => a
  -> d
delegationCred = addrR >>> _.delegation

-- | Converts to cardano-serialization-lib `Address` class object.
toAddressCsl
  :: forall a p d
   . Address a p d
  => a
  -> AddressCsl
toAddressCsl = ToAddressCsl >>> toCslRep

netTagToInt :: NetworkTag -> Int
netTagToInt = case _ of
  TestnetTag -> 0
  MainnetTag -> 1

intToNetTag :: Int -> Maybe NetworkTag
intToNetTag = case _ of
  0 -> Just TestnetTag
  1 -> Just MainnetTag
  _ -> Nothing
