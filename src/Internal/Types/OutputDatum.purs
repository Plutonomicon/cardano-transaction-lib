module CTL.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  , outputDatumDataHash
  , outputDatumDatum
  ) where

import Prelude

import Aeson
  ( class EncodeAeson
  , encodeAeson'
  )
import CTL.Internal.FromData (class FromData, genericFromData)
import CTL.Internal.Helpers (encodeTagged')
import CTL.Internal.ToData (class ToData, genericToData)
import CTL.Internal.TypeLevel.Nat (S, Z)
import CTL.Internal.Types.Datum (Datum)
import CTL.Internal.Types.Transaction (DataHash)
import CTL.Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , PNil
  )
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)

data OutputDatum = NoOutputDatum | OutputDatumHash DataHash | OutputDatum Datum

derive instance Generic OutputDatum _
derive instance Eq OutputDatum

instance Show OutputDatum where
  show = genericShow

instance
  HasPlutusSchema OutputDatum
    ( "NoOutputDatum" := PNil @@ Z
        :+ "OutputDatumHash"
        := PNil
        @@ (S Z)
        :+ "OutputDatum"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData OutputDatum where
  toData = genericToData

instance FromData OutputDatum where
  fromData = genericFromData

instance EncodeAeson OutputDatum where
  encodeAeson' = case _ of
    NoOutputDatum -> encodeAeson' $ encodeTagged' "NoOutputDatum" {}
    OutputDatumHash r -> encodeAeson' $ encodeTagged' "OutputDatumHash" r
    OutputDatum r -> encodeAeson' $ encodeTagged' "OutputDatum" r

outputDatumDataHash :: OutputDatum -> Maybe DataHash
outputDatumDataHash (OutputDatumHash hash) = Just hash
outputDatumDataHash _ = Nothing

outputDatumDatum :: OutputDatum -> Maybe Datum
outputDatumDatum (OutputDatum datum) = Just datum
outputDatumDatum _ = Nothing
