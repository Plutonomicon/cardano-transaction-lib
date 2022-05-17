module Ogmios.Parser (decodeProtocolParameters) where

import Cardano.Api (FromJSON)
import Cardano.Api.Shelley (ProtocolParameters (ProtocolParameters))

import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map

import Data.Char (isDigit)
import Data.Ratio ((%))

import Text.Parsec qualified as Parsec
import Text.Parsec.Char qualified as Parsec.Char
import Text.ParserCombinators.Parsec.Combinator (many1)

newtype ProtocolParametersWrapper = ProtocolParametersWrapper
  { unwrapParams :: ProtocolParameters
  }

instance FromJSON ProtocolParametersWrapper where
  parseJSON =
    Aeson.withObject "ProtocolParametersWrapper" $ \top -> do
      o :: Aeson.Object <- top .: "result"
      v <- o .: "protocolVersion"
      params <-
        ProtocolParameters
          <$> ((,) <$> v .: "major" <*> v .: "minor")
          <*> o .: "decentralizationParameter"
          <*> o .: "extraEntropy"
          <*> o .: "maxBlockHeaderSize"
          <*> o .: "maxBlockBodySize"
          <*> o .: "maxTxSize"
          <*> o .: "minFeeConstant" -- I think minFeeConstant and minFeeCoefficient are swapped here
          -- but this is consistent with the current config file.
          <*> o .: "minFeeCoefficient"
          <*> o .:? "minUTxOValue"
          <*> o .: "stakeKeyDeposit"
          <*> o .: "poolDeposit"
          <*> o .: "minPoolCost"
          <*> o .: "poolRetirementEpochBound"
          <*> o .: "desiredNumberOfPools"
          <*> o .: "poolInfluence"
          <*> o .: "monetaryExpansion"
          <*> o .: "treasuryExpansion"
          <*> o .:? "coinsPerUtxoWord"
          <*> o .:? "costModels" .!= Map.empty
          <*> o .:? "prices"
          <*> o .:? "maxExecutionUnitsPerTransaction"
          <*> o .:? "maxExecutionUnitsPerBlock"
          <*> o .:? "maxValueSize"
          <*> o .:? "collateralPercentage"
          <*> o .:? "maxCollateralInputs"
      return $ ProtocolParametersWrapper params

-- Current Ogmios JSON has structure :
-- {
--  "result" : {
--    "costModels" : {
--      "plutus:v1" :
--    }
--  }
-- }
--
-- But the already defined instance of fromJSON of cardano expect
-- {
--  "result" : {
--    "costModels" : {
--      "PlutusScriptV1" :
--    }
--  }
-- }
-- So, modifyPlutusName changes it to the right name.
modifyPlutusName :: Aeson.Value -> Maybe Aeson.Value
modifyPlutusName (Aeson.Object keyHashMap) =
  do
    result <- HashMap.lookup "result" keyHashMap >>= unwrappObject
    costModels <- HashMap.lookup "costModels" result >>= unwrappObject
    plutus <- HashMap.lookup "plutus:v1" costModels
    let newCostModel =
          HashMap.insert
            "PlutusScriptV1"
            plutus
            (HashMap.delete "plutus:v1" costModels)
        newResult =
          HashMap.insert "costModels" (Aeson.Object newCostModel) result
        newKeyHashMap =
          HashMap.insert "result" (Aeson.Object newResult) keyHashMap
    return $ Aeson.Object newKeyHashMap
  where
    unwrappObject :: Aeson.Value -> Maybe Aeson.Object
    unwrappObject (Aeson.Object obj) = Just obj
    unwrappObject _ = Nothing
modifyPlutusName _ = Nothing

decodeProtocolParameters :: ByteString -> Maybe ProtocolParameters
decodeProtocolParameters response =
  let value :: Maybe Aeson.Value
      value = Aeson.decode response >>= modifyPlutusName
      encoded = Aeson.encode <$> value
      wrapped :: Maybe ProtocolParametersWrapper
      wrapped =
        encoded
          >>= Aeson.decode @ProtocolParametersWrapper
   in unwrapParams <$> wrapped

type LocalParser a = Parsec.Parsec String () a

nonZeroDigit :: LocalParser Char
nonZeroDigit = Parsec.Char.satisfy (\c -> (c /= '0') && isDigit c)

nonZeroInteger :: LocalParser Integer
nonZeroInteger = do
  head <- nonZeroDigit
  remains <- Parsec.many Parsec.Char.digit
  return $ read (head : remains)

zeroInteger :: LocalParser Integer
zeroInteger = read <$> many1 (Parsec.Char.satisfy (== '0'))

haskellInteger :: LocalParser Integer
haskellInteger = nonZeroInteger Parsec.<|> zeroInteger

rational :: LocalParser Rational
rational =
  do
    numerator <- haskellInteger
    Parsec.Char.char '/'
    denominator <- haskellInteger
    return $ numerator % denominator
