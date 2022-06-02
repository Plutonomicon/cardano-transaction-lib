module Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson')
import Aeson.Decode ((</$\>), (</*\>))
import Aeson.Encode ((>/\<))
import Control.Lazy (defer)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import FromData (class FromData, genericFromData)
import Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  )
import ToData (class ToData, genericToData)
import TypeLevel.Nat (S, Z)
import Aeson.Decode as D
import Aeson.Encode as E
import Data.Map as Map
import Serialization.Address (CertificateIndex, Slot, TransactionIndex)
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

-- NOTE: mlabs-haskell/purescript-bridge generated and applied here
instance EncodeAeson Credential where
  encodeAeson' x = encodeAeson' $
    ( defer \_ -> case _ of
        PubKeyCredential a -> E.encodeTagged "PubKeyCredential" a E.value
        ScriptCredential a -> E.encodeTagged "ScriptCredential" a E.value
    ) x

instance DecodeAeson Credential where
  decodeAeson = defer \_ -> D.decode
    $ D.sumType "Credential"
    $ Map.fromFoldable
        [ "PubKeyCredential" /\ D.content (PubKeyCredential <$> D.value)
        , "ScriptCredential" /\ D.content (ScriptCredential <$> D.value)
        ]

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
  encodeAeson' x = encodeAeson' $
    ( defer \_ -> case _ of
        StakingHash a -> E.encodeTagged "StakingHash" a E.value
        StakingPtr ptr -> E.encodeTagged "StakingPtr"
          (ptr.slot /\ ptr.txIx /\ ptr.certIx)
          (E.tuple (E.value >/\< E.value >/\< E.value))
    ) x

instance DecodeAeson StakingCredential where
  decodeAeson = defer \_ -> D.decode
    $ D.sumType "StakingCredential"
    $ Map.fromFoldable
        [ "StakingHash" /\ D.content (StakingHash <$> D.value)
        , "StakingPtr" /\ D.content
            (D.tuple $ toStakingPtr </$\> D.value </*\> D.value </*\> D.value)
        ]
    where
    toStakingPtr slot txIx certIx = StakingPtr { slot, txIx, certIx }
