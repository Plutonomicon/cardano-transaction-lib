module Cardano.Types.OutputDatum
  ( OutputDatum(OutputDatumHash, OutputDatum)
  , outputDatumDataHash
  , outputDatumDatum
  , pprintOutputDatum
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
import Cardano.Types.AsCbor (encodeCbor)
import Cardano.Types.DataHash (DataHash)
import Cardano.Types.PlutusData (PlutusData, pprintPlutusData)
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
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)

data OutputDatum = OutputDatumHash DataHash | OutputDatum PlutusData

derive instance Generic OutputDatum _
derive instance Eq OutputDatum

instance Show OutputDatum where
  show = genericShow

instance
  HasPlutusSchema OutputDatum
    ( "OutputDatumHash"
        := PNil
        @@ Z
        :+ "OutputDatum"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData OutputDatum where
  toData = genericToData

instance FromData OutputDatum where
  fromData = genericFromData

instance EncodeAeson OutputDatum where
  encodeAeson = case _ of
    OutputDatumHash r -> encodeTagged' "OutputDatumHash" r
    OutputDatum r -> encodeTagged' "OutputDatum" r

instance DecodeAeson OutputDatum where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \obj -> do
      tag <- obj .: "tag"
      case tag of
        "OutputDatumHash" -> do
          dataHash <- obj .: "contents"
          pure $ OutputDatumHash dataHash
        "OutputDatum" -> do
          datum <- obj .: "contents"
          pure $ OutputDatum datum
        tagValue -> do
          Left $ UnexpectedValue $ toStringifiedNumbersJson $ fromString
            tagValue

pprintOutputDatum :: OutputDatum -> TagSet
pprintOutputDatum = TagSet.fromArray <<< case _ of
  OutputDatumHash hash ->
    [ "datumHash" `tag` byteArrayToHex (unwrap $ encodeCbor hash) ]
  OutputDatum d ->
    [ "datum" `tagSetTag` pprintPlutusData d ]

outputDatumDataHash :: OutputDatum -> Maybe DataHash
outputDatumDataHash (OutputDatumHash hash) = Just hash
outputDatumDataHash _ = Nothing

outputDatumDatum :: OutputDatum -> Maybe PlutusData
outputDatumDatum (OutputDatum datum) = Just datum
outputDatumDatum _ = Nothing
