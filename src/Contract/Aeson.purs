-- | A module to write Aeson instances. The module replicates DecodeJson but
-- | but provides safely for parsing certain values e.g. `BigInt`.
module Contract.Aeson (module Aeson) where

import Aeson
  ( (.:)
  , (.:?)
  , Aeson
  , AesonCases
  , AesonEncoder
  , NumberIndex
  , class EncodeAeson
  , class GEncodeAeson
  , class DecodeAeson
  , class DecodeAesonField
  , class GDecodeAeson
  , bumpNumberIndexBy
  , caseAeson
  , caseAesonArray
  , caseAesonBigInt
  , caseAesonBoolean
  , caseAesonNull
  , caseAesonNumber
  , caseAesonObject
  , caseAesonString
  , caseAesonUInt
  , constAesonCases
  , decodeAeson
  , decodeAesonField
  , decodeAesonViaJson
  , decodeJsonString
  , encodeAeson
  , encodeAeson'
  , encodeAesonViaJson
  , gDecodeAeson
  , gEncodeAeson
  , useNextIndexIndex
  , getCurrentNumberIndex
  , getField
  , getFieldOptional
  , getFieldOptional'
  , getNestedAeson
  , getNumberIndex
  , jsonToAeson
  , parseJsonStringToAeson
  , stringifyAeson
  , toObject
  , toStringifiedNumbersJson
  ) as Aeson
