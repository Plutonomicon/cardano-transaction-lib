module Types.PlutusAddress where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (head, take, drop)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (wrap)
import Data.Int.Bits (and, shr) as Bits
import Types.ByteArray (ByteArray, byteArrayToIntArray, byteArrayFromIntArray)
import Types.UnbalancedTransaction (PubKeyHash)
import Types.Scripts (ValidatorHash)
import Types.PlutusData (PlutusData(Constr, Map, List, Integer, Bytes))
import Serialization.Address (Pointer, addressBytes)
import Serialization.Address (Address) as Serialization
import Serialization.Hash (ed25519KeyHashFromBytes, scriptHashFromBytes) as Hash
import ToData (class ToData, toData)
import FromData (class FromData, fromData)

--------------------------------------------------------------------------------
-- Credential
--------------------------------------------------------------------------------

data Credential
  = PubKeyCredential PubKeyHash
  | ScriptCredential ValidatorHash

derive instance Generic Credential _
instance Show Credential where
  show = genericShow

instance ToData Credential where
  toData (PubKeyCredential pubKeyHash) =
    Constr zero [ toData pubKeyHash ]
  toData (ScriptCredential validatorHash) =
    Constr one [ toData validatorHash ]

instance FromData Credential where
  fromData (Constr n [ pd ])
    | n == zero = PubKeyCredential <$> fromData pd
    | n == one = ScriptCredential <$> fromData pd
  fromData _ = Nothing

--------------------------------------------------------------------------------
-- StakingCredential
--------------------------------------------------------------------------------

data StakingCredential
  = StakingHash Credential
  | StakingPtr Pointer

derive instance Generic StakingCredential _

instance Show StakingCredential where
  show = genericShow

instance ToData StakingCredential where
  toData (StakingHash credential) =
    Constr zero [ toData credential ]
  toData (StakingPtr ptr) =
    Constr one [ toData ptr.slot, toData ptr.txIx, toData ptr.certIx ]

instance FromData StakingCredential where
  fromData (Constr zero [ pd ]) =
    StakingHash <$> fromData pd
  fromData (Constr one [ slotD, txIxD, certIxD ]) =
    StakingPtr <$> ({ slot: _, txIx: _, certIx: _} <$>
      fromData slotD <*> fromData txIxD <*> fromData certIxD)
  fromData _ = Nothing

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

data PlutusAddress = Address
  { addressCredential :: Credential
  , addressStakingCredential :: Maybe StakingCredential
  }

derive instance Generic PlutusAddress _

instance Show PlutusAddress where
  show = genericShow

instance ToData PlutusAddress where
  toData (Address a) =
    Constr zero [ toData a.addressCredential, toData a.addressStakingCredential ]

instance FromData PlutusAddress where
  fromData (Constr zero [ credD, stakingCredD ]) =
    Address <$> ({ addressCredential: _, addressStakingCredential: _ } <$>
      fromData credD <*> fromData stakingCredD)
  fromData _ = Nothing

--------------------------------------------------------------------------------
-- IsoNativeForeign
--------------------------------------------------------------------------------

class IsoNativeForeign native foreign' | native -> foreign', foreign' -> native where
  toNativeType :: foreign' -> Maybe native
-- TODO: toForeignType :: native -> foreign'

instance IsoNativeForeign PlutusAddress Serialization.Address where
  toNativeType addrForeign = addrType >>= \addrType' ->
    case addrType' of -- TODO: Byron addresses, `Pointer` delegation part
      -- %b0000 | network tag | key hash | key hash
      0 -> buildAddress pubKeyCredential $ Just $
        map StakingHash <<< pubKeyCredential

      -- %b0001 | network tag | script hash | key hash
      1 -> buildAddress scriptCredential $ Just $
        map StakingHash <<< pubKeyCredential

      -- %b0010 | network tag | key hash | script hash
      2 -> buildAddress pubKeyCredential $ Just $
        map StakingHash <<< scriptCredential

      -- %b0011 | network tag | script hash | script hash
      3 -> buildAddress scriptCredential $ Just $
        map StakingHash <<< scriptCredential

      -- %b0110 | network tag | key hash
      6 -> buildAddress pubKeyCredential Nothing

      -- %b0111 | network tag | script hash
      7 -> buildAddress scriptCredential Nothing

      _ -> Nothing
    where
    addrBytes :: Array Int
    addrBytes = byteArrayToIntArray $ addressBytes addrForeign

    addrType :: Maybe Int
    addrType = head addrBytes >>= (pure <<< flip Bits.shr 4)

    pubKeyCredential :: ByteArray -> Maybe Credential
    pubKeyCredential =
      map (PubKeyCredential <<< wrap) <<< Hash.ed25519KeyHashFromBytes

    scriptCredential :: ByteArray -> Maybe Credential
    scriptCredential =
      map (ScriptCredential <<< wrap) <<< Hash.scriptHashFromBytes

    paymentPartHash :: Maybe ByteArray
    paymentPartHash = byteArrayFromIntArray $ take 28 (drop 1 addrBytes)

    delegationPartHash :: Maybe ByteArray
    delegationPartHash = byteArrayFromIntArray $ drop 29 addrBytes

    buildAddress
      :: (ByteArray -> Maybe Credential)
      -> Maybe (ByteArray -> Maybe StakingCredential)
      -> Maybe PlutusAddress
    buildAddress credential stakingCredential =
      paymentPartHash >>= credential >>= \c ->
        case stakingCredential of
          Nothing -> Just $
            Address { addressCredential: c, addressStakingCredential: Nothing }
          Just stakingCredential ->
            delegationPartHash >>= stakingCredential >>= \sc -> Just $
              Address { addressCredential: c, addressStakingCredential: Just sc }