module Ctl.Internal.Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(..)
  , decodeAeson
  , (.:)
  )
import Contract.Prelude (type (/\))
import Ctl.Internal.FromData (class FromData, genericFromData)
import Ctl.Internal.Helpers (contentsProp, encodeTagged', tagProp)
import Ctl.Internal.Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  )
import Ctl.Internal.Serialization.Address
  ( CertificateIndex
  , Slot
  , TransactionIndex
  )
import Ctl.Internal.ToData (class ToData, genericToData)
import Ctl.Internal.TypeLevel.Nat (S, Z)
import Ctl.Internal.Types.PubKeyHash (PubKeyHash)
import Ctl.Internal.Types.Scripts (ValidatorHash)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))

--------------------------------------------------------------------------------
-- Credential
--------------------------------------------------------------------------------

-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Api.html#t:Credential
-- Plutus rev: dbefda30be6490c758aa88b600f5874f12712b3a
-- | Credential required to unlock a transaction output.
data Credential
  -- | The transaction that spends this output must be signed by
  -- | the private key.
  = PubKeyCredential PubKeyHash
  -- | The transaction that spends this output must include the validator
  -- | script and be accepted by the validator.
  | ScriptCredential ValidatorHash

derive instance Eq Credential
derive instance Ord Credential
derive instance Generic Credential _

instance Show Credential where
  show = genericShow

instance
  HasPlutusSchema
    Credential
    ( "PubKeyCredential" := PNil @@ Z
        :+ "ScriptCredential"
        := PNil
        @@ (S Z)
        :+ PNil
    )

-- NOTE: mlabs-haskell/purescript-bridge generated and applied here
instance EncodeAeson Credential where
  encodeAeson = case _ of
    PubKeyCredential a -> encodeTagged' "PubKeyCredential" a
    ScriptCredential a -> encodeTagged' "ScriptCredential" a

instance DecodeAeson Credential where
  decodeAeson a = lmap (Named "Credential") do
    obj <- decodeAeson a
    tag <- obj .: tagProp
    case tag of
      "PubKeyCredential" -> PubKeyCredential <$> obj .: contentsProp
      "ScriptCredential" -> ScriptCredential <$> obj .: contentsProp
      _ -> Left $ AtKey tagProp $ UnexpectedValue $ encodeString tag

instance ToData Credential where
  toData = genericToData

instance FromData Credential where
  fromData = genericFromData

--------------------------------------------------------------------------------
-- StakingCredential
--------------------------------------------------------------------------------

-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Api.html#t:StakingCredential
-- Plutus rev: dbefda30be6490c758aa88b600f5874f12712b3a
-- | Staking credential used to assign rewards.
data StakingCredential
  = StakingHash Credential
  | StakingPtr
      { slot :: Slot
      , txIx :: TransactionIndex
      , certIx :: CertificateIndex
      }

derive instance Eq StakingCredential
derive instance Ord StakingCredential
derive instance Generic StakingCredential _

instance Show StakingCredential where
  show = genericShow

instance
  HasPlutusSchema
    StakingCredential
    ( "StakingHash" := PNil @@ Z
        :+ "StakingPtr"
        :=
          ( "slot" := I Slot :+ "txIx" := I TransactionIndex :+ "certIx"
              := I CertificateIndex
              :+ PNil
          )
        @@ (S Z)
        :+ PNil
    )

instance ToData StakingCredential where
  toData = genericToData

instance FromData StakingCredential where
  fromData = genericFromData

-- NOTE: mlabs-haskell/purescript-bridge generated and applied here
instance EncodeAeson StakingCredential where
  encodeAeson = case _ of
    StakingHash a -> encodeTagged' "StakingHash" a
    StakingPtr ptr -> encodeTagged' "StakingPtr"
      (ptr.slot /\ ptr.txIx /\ ptr.certIx)

instance DecodeAeson StakingCredential where
  decodeAeson a = lmap (Named "StakingCredential") do
    obj <- decodeAeson a
    tag <- obj .: tagProp
    case tag of
      "StakingHash" -> StakingHash <$> obj .: contentsProp
      "StakingPtr" -> toStakingPtr <$> obj .: contentsProp
      _ -> Left $ AtKey tagProp $ UnexpectedValue $ encodeString tag
    where
    toStakingPtr
      :: (Slot /\ TransactionIndex /\ CertificateIndex) -> StakingCredential
    toStakingPtr (slot /\ txIx /\ certIx) = StakingPtr { slot, txIx, certIx }