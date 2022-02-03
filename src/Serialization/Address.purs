module Serialization.Address
  ( BaseAddressCsl
  , BaseAddress
  , NetworkTag
  , StakeCred
  , CredType
  , StakeCredentialCsl
  , addressBech32
  , addressBytes
  , addressFromBech32
  , addressFromBytes
  , anyCredType
  , ed25519KeyHashCredType
  , mkBaseAddress
  , netTagToInt
  , pubKeyAddress
  , scriptAddress
  , scriptHashCredType
  ) where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Typelevel.Undefined (undefined)
import FFiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization.Bech32 (Bech32String)
import Serialization.Csl (class ToCsl, toCslRep)
import Serialization.Hash (Ed25519KeyHash, ScriptHash)

---- assumed this data types and that they are correct by construction ----

-- | Denotes one of two possible values of Shelley address' network tag.
data NetworkTag = TestnetTag | MainnetTag

netTagToInt :: NetworkTag -> Int
netTagToInt = case _ of
  TestnetTag -> 0
  MainnetTag -> 1

derive instance Eq NetworkTag

instance Show NetworkTag where
  show = case _ of
    TestnetTag -> "TestnetTag"
    MainnetTag -> "MainnetTag"

-- | Simple safety wrapper to ensure that only valid types are provided
-- | as address' stake credentials.
newtype StakeCred a = StakeCred a

foreign import data StakeCredentialCsl :: Type
foreign import stakeCredFromKeyHash :: Ed25519KeyHash -> StakeCredentialCsl
foreign import stakeCredFromScriptHash :: ScriptHash -> StakeCredentialCsl

instance ToCsl (StakeCred Ed25519KeyHash) StakeCredentialCsl where
  toCslRep (StakeCred x) = stakeCredFromKeyHash x

instance ToCsl (StakeCred ScriptHash) StakeCredentialCsl where
  toCslRep (StakeCred x) = stakeCredFromScriptHash x

-- | Shelley-era address where payment and delegation parts
-- | are each either a `Ed25519Keyhash` or a `ScriptHash`
newtype BaseAddress (p :: Type) (d :: Type) = BaseAddress
  { network :: NetworkTag
  , payment :: p
  , delegation :: d
  }

-- | BaseAddress CSL representation
foreign import data BaseAddressCsl :: Type
foreign import newBaseAddressCsl
  :: { network :: Int
     , paymentStakeCred :: StakeCredentialCsl
     , delegationStakeCred :: StakeCredentialCsl
     }
  -> BaseAddressCsl

-- | With this instance ou can safely convert BaseAddress to csl rep
-- | to use csl functions
instance (ToCsl (StakeCred d) StakeCredentialCsl, ToCsl (StakeCred p) StakeCredentialCsl) => ToCsl (BaseAddress p d) BaseAddressCsl where
  toCslRep (BaseAddress { network, payment, delegation }) = newBaseAddressCsl
    { network: netTagToInt network
    , paymentStakeCred: toCslRep (StakeCred payment)
    , delegationStakeCred: toCslRep (StakeCred delegation)
    }

-- | Address where payment and stake delegation rights are bound to the same PubKeyHash
type PubKeyAddress = BaseAddress Ed25519KeyHash Ed25519KeyHash

-- | Address where payment and stake delegation rights are bound to the same ScriptHash
type ScriptAddress = BaseAddress ScriptHash ScriptHash

-- | Helper for reclaiming `BaseAddress` type arguments during decoding.
newtype CredType (p :: Type) = CredType (Maybe Boolean)

---- address <->

-- | The exported constructor for `BaseAddress`.
mkBaseAddress
  :: forall p d
   . (ToCsl (StakeCred d) StakeCredentialCsl)
  => (ToCsl (StakeCred p) StakeCredentialCsl)
  => { network :: NetworkTag, payment :: p, delegation :: d }
  -> BaseAddress p d
mkBaseAddress { network, payment, delegation } = BaseAddress
  { network: network
  , payment: payment
  , delegation: delegation
  }

foreign import addressBytesImpl :: BaseAddressCsl -> Uint8Array
foreign import headerCheck
  :: forall p d
   . MaybeFfiHelper
  -> { payment :: CredType p
     , delegation :: CredType d
     }
  -> Uint8Array
  -> BaseAddressCsl
  -> Maybe (BaseAddress p d)

foreign import addressFromBytesImpl :: MaybeFfiHelper -> Uint8Array -> Maybe BaseAddressCsl
foreign import addressFromBech32Impl :: MaybeFfiHelper -> Bech32String -> Maybe BaseAddressCsl

-- | Convert address to its byte representation
addressBytes
  :: forall p d
   . (ToCsl (StakeCred d) StakeCredentialCsl)
  => (ToCsl (StakeCred p) StakeCredentialCsl)
  => BaseAddress p d
  -> Uint8Array
addressBytes = toCslRep >>> addressBytesImpl

-- | Parse `BaseAddress` from its byte representation
addressFromBytes
  :: forall p d
   . { payment :: CredType p
     , delegation :: CredType d
     }
  -> Uint8Array
  -> Maybe (BaseAddress p d)
addressFromBytes checks bts = do
  addrCsl <- addressFromBytesImpl maybeFfiHelper bts
  headerCheck maybeFfiHelper checks bts addrCsl

-- | Return Cardano Bech32 representation of BaseAddress
-- NOTE. Use csl implementation which apparently can fail.
-- is it possible to guarantee safety?
addressBech32 :: forall p d. BaseAddress p d -> Maybe Bech32String
addressBech32 = undefined

-- | Build address out of its bech32 representation.
addressFromBech32 :: forall p d. { payment :: CredType p, delegation :: CredType d } -> Bech32String -> Maybe (BaseAddress p d)
addressFromBech32 checks bchString = do
  addrCsl <- addressFromBech32Impl maybeFfiHelper bchString
  let bts = addressBytesImpl addrCsl
  headerCheck maybeFfiHelper checks bts addrCsl

-- | Build `PubKeyAddress` from a single credential.
pubKeyAddress :: NetworkTag -> Ed25519KeyHash -> PubKeyAddress
pubKeyAddress nt pkh = BaseAddress { network: nt, payment: pkh, delegation: pkh }

-- | Build `ScriptAddress` from a single credential.
scriptAddress :: NetworkTag -> ScriptHash -> ScriptAddress
scriptAddress nt sh = BaseAddress { network: nt, payment: sh, delegation: sh }

---- cred types

-- | Helper to set BaseAddress type arguments during decoding.
anyCredType :: CredType (forall a. a)
anyCredType = CredType Nothing

-- | Helper to set BaseAddress type arguments during decoding.
ed25519KeyHashCredType :: CredType Ed25519KeyHash
ed25519KeyHashCredType = CredType (Just false)

-- | Helper to set BaseAddress type arguments during decoding.
scriptHashCredType :: CredType ScriptHash
scriptHashCredType = CredType (Just true)
