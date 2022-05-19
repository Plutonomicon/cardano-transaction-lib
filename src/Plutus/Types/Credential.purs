module Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  ) where

import Prelude

import Serialization.Address
  ( CertificateIndex(..)
  , Slot(..)
  , TransactionIndex(..)
  )
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Show.Generic (genericShow)
import FromData (class FromData, fromData, genericFromData)
import Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  )
import Serialization.Address (Pointer)
import ToData (class ToData, genericToData, toData)
import TypeLevel.Nat (S, Z)
import Types.PlutusData (PlutusData(Constr))
import Types.PubKeyHash (PubKeyHash)
import Types.Scripts (ValidatorHash)

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
