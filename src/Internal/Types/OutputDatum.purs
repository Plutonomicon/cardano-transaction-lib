module Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  , outputDatumDataHash
  , outputDatumDatum
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch, UnexpectedValue)
  , caseAesonObject
  , fromString
  , toStringifiedNumbersJson
  , (.:)
  )
import Ctl.Internal.FromData (class FromData, genericFromData)
import Ctl.Internal.Helpers (encodeTagged')
import Ctl.Internal.Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , PNil
  )
import Ctl.Internal.ToData (class ToData, genericToData)
import Ctl.Internal.TypeLevel.Nat (S, Z)
import Ctl.Internal.Types.Datum (Datum)
import Ctl.Internal.Types.Transaction (DataHash)
import Data.Either (Either(Left))
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
  encodeAeson = case _ of
    NoOutputDatum -> encodeTagged' "NoOutputDatum" {}
    OutputDatumHash r -> encodeTagged' "OutputDatumHash" r
    OutputDatum r -> encodeTagged' "OutputDatum" r

instance DecodeAeson OutputDatum where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \obj -> do
      tag <- obj .: "tag"
      case tag of
        "NoOutputDatum" ->
          pure NoOutputDatum
        "OutputDatumHash" -> do
          dataHash <- obj .: "contents"
          pure $ OutputDatumHash dataHash
        "OutputDatum" -> do
          datum <- obj .: "contents"
          pure $ OutputDatumHash datum
        tagValue -> do
          Left $ UnexpectedValue $ toStringifiedNumbersJson $ fromString
            tagValue

outputDatumDataHash :: OutputDatum -> Maybe DataHash
outputDatumDataHash (OutputDatumHash hash) = Just hash
outputDatumDataHash _ = Nothing

outputDatumDatum :: OutputDatum -> Maybe Datum
outputDatumDatum (OutputDatum datum) = Just datum
outputDatumDatum _ = Nothing
