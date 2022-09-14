module CTL.Internal.Plutus.Types.Credential
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
import CTL.Internal.FromData (class FromData, genericFromData)
import CTL.Internal.Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  )
import CTL.Internal.ToData (class ToData, genericToData)
import CTL.Internal.TypeLevel.Nat (S, Z)
import Aeson.Decode as Decode
import Aeson.Encode as Encode
import Data.Map as Map
import CTL.Internal.Serialization.Address (CertificateIndex, Slot, TransactionIndex)
import CTL.Internal.Types.PubKeyHash (PubKeyHash)
import CTL.Internal.Types.Scripts (ValidatorHash)

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
  encodeAeson' = encodeAeson' <<<
    ( defer $ const $ case _ of
        PubKeyCredential a -> Encode.encodeTagged "PubKeyCredential" a
          Encode.value
        ScriptCredential a -> Encode.encodeTagged "ScriptCredential" a
          Encode.value
    )

instance DecodeAeson Credential where
  decodeAeson = defer $ const $ Decode.decode
    $ Decode.sumType "Credential"
    $ Map.fromFoldable
        [ "PubKeyCredential" /\ Decode.content
            (PubKeyCredential <$> Decode.value)
        , "ScriptCredential" /\ Decode.content
            (ScriptCredential <$> Decode.value)
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
  encodeAeson' = encodeAeson' <<< defer
    ( const $ case _ of
        StakingHash a -> Encode.encodeTagged "StakingHash" a Encode.value
        StakingPtr ptr -> Encode.encodeTagged "StakingPtr"
          (ptr.slot /\ ptr.txIx /\ ptr.certIx)
          (Encode.tuple (Encode.value >/\< Encode.value >/\< Encode.value))
    )

instance DecodeAeson StakingCredential where
  decodeAeson = defer $ const $ Decode.decode
    $ Decode.sumType "StakingCredential"
    $ Map.fromFoldable
        [ "StakingHash" /\ Decode.content (StakingHash <$> Decode.value)
        , "StakingPtr" /\ Decode.content
            ( Decode.tuple $ toStakingPtr </$\> Decode.value </*\> Decode.value
                </*\> Decode.value
            )
        ]
    where
    toStakingPtr
      :: Slot -> TransactionIndex -> CertificateIndex -> StakingCredential
    toStakingPtr slot txIx certIx = StakingPtr { slot, txIx, certIx }
