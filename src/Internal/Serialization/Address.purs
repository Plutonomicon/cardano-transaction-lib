module Ctl.Internal.Serialization.Address
  ( BlockId(BlockId)
  , TransactionIndex(TransactionIndex)
  , CertificateIndex(CertificateIndex)
  -- , addressBech32
  -- , addressNetworkId
  -- , keyHashCredential
  -- , scriptHashCredential
  -- , withStakeCredential
  -- , baseAddress
  -- , baseAddressPaymentCred
  -- , baseAddressDelegationCred
  -- , baseAddressToAddress
  , ByronProtocolMagic(ByronProtocolMagic)
  ) where

import Cardano.Types.NetworkId
import Cardano.Types.Slot
import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.BaseAddress as X
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.ByronAddress as X
import Cardano.Types.EnterpriseAddress as X
import Cardano.Types.NetworkId as NetworkId
import Cardano.Types.NetworkId as X
import Cardano.Types.PlutusData (PlutusData(Bytes))
import Cardano.Types.Pointer as X
import Cardano.Types.PointerAddress as X
import Cardano.Types.RewardAddress as X
import Cardano.Types.Slot as X
import Cardano.Types.StakeCredential as X
import Control.Alt ((<|>))
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Helpers (encodeTagged')
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash, ScriptHash)
import Ctl.Internal.Serialization.Types (Bip32PublicKey)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.Aliases (Base58String, Bech32String)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Data.ByteArray (ByteArray)
import Data.Either (note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)
import Partial.Unsafe (unsafePartial)

-- it is an integer in ogmios
-- bytestring in plutus
-- uint32 in csl
newtype BlockId = BlockId UInt

derive newtype instance Eq BlockId
derive instance Newtype BlockId _
derive instance Generic BlockId _

instance EncodeAeson BlockId where
  encodeAeson (BlockId id) = encodeAeson id

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

newtype ByronProtocolMagic = ByronProtocolMagic UInt
