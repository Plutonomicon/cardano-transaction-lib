module Ogmios.Parser (decodeProtocolParameters) where

import Cardano.Api (ExecutionUnitPrices (ExecutionUnitPrices), ExecutionUnits (ExecutionUnits), FromJSON, Lovelace (Lovelace))
import Cardano.Api.Shelley (ProtocolParameters (ProtocolParameters), makePraosNonce)

import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import GHC.Natural (Natural, naturalFromInteger)

import Data.Char (isDigit)
import Data.Ratio ((%))

import Text.Parsec qualified as Parsec
import Text.Parsec.Char qualified as Parsec.Char
import Text.ParserCombinators.Parsec.Combinator (many1)

import Data.Aeson (Object, (.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.BetterErrors

import Data.Text qualified as Text
import Data.Void (Void)

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

getResultObject :: Parse e Object
getResultObject = key "result" asObject

parseVersion :: Parse e (Natural, Natural)
parseVersion =
  key "protocolVersion" $
    (,) <$> parseNatural "major" <*> parseNatural "minor"

parseNatural :: Text.Text -> Parse e Natural
parseNatural strKey =
  naturalFromInteger . toInteger <$> key strKey asIntegral

parseLovelace :: Text.Text -> Parse e Lovelace
parseLovelace strKey =
  key strKey $ 
    Lovelace . toInteger <$> asIntegral

parseRational :: Text.Text -> Parse Text.Text Rational
parseRational strKey =
  key strKey (withString rationalParser)


parseExecutionPrices :: Parse Text.Text (Maybe ExecutionUnitPrices)
parseExecutionPrices =
  perhaps $
    ExecutionUnitPrices
      <$> parseRational "steps"
      <*> parseRational "memory"

parseExecutionUnits :: Parse e (Maybe ExecutionUnits)
parseExecutionUnits = fromAesonParser

--  perhaps $ ExecutionUnits
--    <$> parseRational "steps"
--    <*> parseRational "memory"
--
parseResult :: Parse Text.Text ProtocolParameters
parseResult =
  key "result" $
    ProtocolParameters
      <$> parseVersion
      <*> parseRational "decentralizationParameter"
      -- TODO : How to parse the value `neutral` from ogmios? 
      <*> pure Nothing -- key "extraEntropy" (perhaps (makePraosNonce . BLU.fromString <$>asString))
      <*> parseNatural "maxBlockHeaderSize"
      <*> parseNatural "maxBlockBodySize"
      <*> parseNatural "maxTxSize"
      <*> parseNatural "minFeeConstant" -- I think minFeeConstant and minFeeCoefficient are swapped here
      -- but this is consistent with the current config file.
      <*> parseNatural "minFeeCoefficient"
      <*> pure Nothing
      <*> parseLovelace "stakeKeyDeposit"
      <*> parseLovelace "poolDeposit"
      <*> parseLovelace "minPoolCost"
      <*> key "poolRetirementEpochBound" fromAesonParser
      <*> parseNatural "desiredNumberOfPools"
      <*> parseRational "poolInfluence"
      <*> parseRational "monetaryExpansion"
      <*> parseRational "treasuryExpansion"
      <*> perhaps (parseLovelace "coinsPerUtxoWord")
      -- TODO : Ogmios uses a scriptname different than cardano 
      -- to plutus, this would make this parameter to fail.
      <*> (key "costModels" fromAesonParser <|> pure Map.empty)
      <*> key "prices" parseExecutionPrices
      <*> key "maxExecutionUnitsPerTransaction" parseExecutionUnits
      <*> key "maxExecutionUnitsPerBlock" parseExecutionUnits
      <*> perhaps (parseNatural "maxValueSize")
      <*> perhaps (parseNatural "collateralPercentage")
      <*> perhaps (parseNatural "maxCollateralInputs")

decodeProtocolParameters :: ByteString -> Either [Text.Text] ProtocolParameters
decodeProtocolParameters response =
  let value :: Maybe Aeson.Value
      value = Aeson.decode response
   in case parseValue parseResult <$> value of
        Just (Right params) -> Right params
        Just (Left e) -> Left $ displayError id e
        _ -> Left ["Fail at converting ogmios response to cardano format"]

--      wrapped :: Maybe ProtocolParametersWrapper
--      wrapped =
--        encoded
--          >>= Aeson.decode @ProtocolParametersWrapper
--   in unwrapParams <$> wrapped

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
    Parsec.Char.spaces
    numerator <- haskellInteger
    Parsec.Char.spaces
    Parsec.Char.char '/'
    Parsec.Char.spaces
    denominator <- haskellInteger
    return $ numerator % denominator

rationalParser :: String -> Either Text.Text Rational
rationalParser s =
  case Parsec.runParser rational () "ogmios.json" s of
    Right value -> Right value
    Left _ -> Left "can't parse Rational"
