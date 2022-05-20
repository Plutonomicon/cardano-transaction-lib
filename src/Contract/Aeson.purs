-- | A module to write Aeson instances. The module replicates DecodeJson but
-- | but provides safely for parsing certain values e.g. `BigInt`.
module Contract.Aeson (module Aeson) where

import Aeson
  ( class DecodeAeson
  , class DecodeAesonField
  , class EncodeAeson
  , class GDecodeAeson
  , class GEncodeAeson
  , Aeson
  , AesonCases
  , AesonEncoder
  , JsonDecodeError
      ( TypeMismatch
      , UnexpectedValue
      , AtIndex
      , AtKey
      , Named
      , MissingValue
      )
  , NumberIndex
  , aesonNull
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
  , decodeTraversable
  , encodeAeson
  , encodeAeson'
  , encodeAesonViaJson
  , encodeTraversable
  , fromString
  , gDecodeAeson
  , gEncodeAeson
  , getCurrentNumberIndex
  , getField
  , getFieldOptional
  , getFieldOptional'
  , getNestedAeson
  , getNumberIndex
  , isArray
  , isBoolean
  , isNull
  , isNumber
  , isObject
  , isString
  , jsonToAeson
  , parseJsonStringToAeson
  , stringifyAeson
  , toArray
  , toBoolean
  , toNull
  , toNumber
  , toObject
  , toString
  , toStringifiedNumbersJson
  , useNextIndexIndex
  , (.:)
  , (.:?)
  ) as Aeson
