module Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  ) where

import Prelude
import Data.Maybe (Maybe(Nothing))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Types.Scripts (ValidatorHash)
import Types.PlutusData (PlutusData(Constr))
import Types.PubKeyHash (PubKeyHash)
import Serialization.Address (Pointer)
import ToData (class ToData, toData)
import FromData (class FromData, fromData)

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

-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Api.html#t:StakingCredential
-- Plutus rev: dbefda30be6490c758aa88b600f5874f12712b3a
-- | Staking credential used to assign rewards.
data StakingCredential
  = StakingHash Credential
  | StakingPtr Pointer

derive instance Eq StakingCredential
derive instance Ord StakingCredential
derive instance Generic StakingCredential _

instance Show StakingCredential where
  show = genericShow

instance ToData StakingCredential where
  toData (StakingHash credential) =
    Constr zero [ toData credential ]
  toData (StakingPtr ptr) =
    Constr one [ toData ptr.slot, toData ptr.txIx, toData ptr.certIx ]

instance FromData StakingCredential where
  fromData (Constr n [ pd ]) | n == zero =
    StakingHash <$> fromData pd
  fromData (Constr n [ slotD, txIxD, certIxD ]) | n == one =
    StakingPtr <$>
      ( { slot: _, txIx: _, certIx: _ }
          <$> fromData slotD
          <*> fromData txIxD
          <*> fromData certIxD
      )
  fromData _ = Nothing
