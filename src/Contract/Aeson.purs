-- | A module to write Aeson instances. The module replicates DecodeJson but
-- | but provides safely for parsing certain values e.g. `BigInt`.
module Contract.Aeson (module Aeson) where

import Aeson
  ( NumberIndex
  , class DecodeAeson
  , class DecodeAesonField
  , class GDecodeAeson
  , Aeson
  , (.:)
  , (.:?)
  , AesonCases
  , caseAeson
  , caseAesonArray
  , caseAesonBoolean
  , caseAesonNull
  , caseAesonObject
  , caseAesonString
  , caseAesonNumber
  , caseAesonUInt
  , caseAesonBigInt
  , constAesonCases
  , decodeAeson
  , decodeAesonField
  , decodeJsonString
  , gDecodeAeson
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