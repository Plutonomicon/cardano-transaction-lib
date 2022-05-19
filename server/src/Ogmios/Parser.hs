module Ogmios.Parser (decodeProtocolParameters) where

import Cardano.Api (
  AnyPlutusScriptVersion (AnyPlutusScriptVersion),
  CostModel,
  ExecutionUnitPrices (ExecutionUnitPrices),
  ExecutionUnits,
  Lovelace (Lovelace),
  PlutusScriptVersion (PlutusScriptV1),
  PraosNonce,
 )
import Cardano.Api.Shelley (ProtocolParameters (ProtocolParameters))
import Data.Aeson qualified as Aeson
import Data.Aeson.BetterErrors (
  Parse,
  asIntegral,
  displayError,
  fromAesonParser,
  key,
  parseValue,
  perhaps,
  withString,
 )
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString, pack, unpack)
import Data.ByteString.Lazy.UTF8 qualified as UTF8
import Data.Char (isDigit)
import Data.Map qualified as Map
import Data.Ratio ((%))
import Data.Text qualified as Text
import GHC.Natural (Natural, naturalFromInteger)
import GHC.Word (Word8)

import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.HashMap.Strict qualified as HashMap

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser, inClass, many', many1, satisfy, string)

newtype ProtocolParametersWrapper = ProtocolParametersWrapper
  { unwrapParams :: ProtocolParameters
  }

instance Aeson.FromJSON ProtocolParametersWrapper where
  parseJSON =
    Aeson.withObject "ProtocolParametersWrapper" $ \top -> do
      o :: Aeson.Object <- top .: "result"
      v <- o .: "protocolVersion"
      params <-
        ProtocolParameters
          <$> ((,) <$> v .: "major" <*> v .: "minor")
          <*> Aeson.Types.explicitParseField (const rationalParser) o "decentralizationParameter"
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

parseVersion :: Parse e (Natural, Natural)
parseVersion =
  key "protocolVersion" $
    (,) <$> parseNatural "major" <*> parseNatural "minor"

parseNatural :: Text.Text -> Parse e Natural
parseNatural strKey =
  naturalFromInteger <$> key strKey asIntegral

parseLovelace :: Text.Text -> Parse e Lovelace
parseLovelace strKey =
  key strKey $
    Lovelace <$> asIntegral

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

parseCostModels :: Parse e (Map.Map AnyPlutusScriptVersion CostModel)
parseCostModels =
  Map.singleton (AnyPlutusScriptVersion PlutusScriptV1)
    <$> key "plutus:v1" fromAesonParser

{- | `extraEntropy` has been added as part of the
 cardano decentralization
 https://iohk.io/en/blog/posts/2021/03/29/the-secure-transition-to-decentralization/
 (more details on cip 9)
 It must be a Hex encoded string, but currently is
 set as "neutralNonce" (ogmios : "neutral) making the hex parser from
 Aeson to fail.
 From code we can see that currently a Nothing value
 is translated to `Ledger.NeutralNonce`
-}
parsePraosNonce :: Text.Text -> Parse Text.Text (Maybe PraosNonce)
parsePraosNonce strKey =
  perhaps (key strKey fromAesonParser)
    <|> key strKey (withString neutralNonceParser)

parseResult :: Parse Text.Text ProtocolParameters
parseResult =
  key "result" $
    ProtocolParameters
      <$> parseVersion
      <*> parseRational "decentralizationParameter"
      <*> parsePraosNonce "extraEntropy"
      <*> parseNatural "maxBlockHeaderSize"
      <*> parseNatural "maxBlockBodySize"
      <*> parseNatural "maxTxSize"
      <*> parseNatural "minFeeConstant"
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
      <*> (key "costModels" parseCostModels <|> pure Map.empty)
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

nonZeroDigit :: Parser Word8
nonZeroDigit = satisfy $ inClass "123456789"

digit :: Parser Word8
digit = satisfy $ inClass "0123456789"

nonZeroInteger :: Parser Integer
nonZeroInteger = do
  headDigit <- nonZeroDigit
  remains <- many' digit
  pure . read . UTF8.toString $ pack (headDigit : remains)

zeroInteger :: Parser Integer
zeroInteger = read . UTF8.toString . pack <$> many1 (satisfy $ inClass "0")

haskellInteger :: Parser Integer
haskellInteger = nonZeroInteger <|> zeroInteger

rationalParser :: Parser Rational
rationalParser = do
  numerator <- haskellInteger
  _ <- satisfy $ inClass "/"
  denominator <- haskellInteger
  pure $ numerator % denominator

neutralNonceParser :: Parser (Maybe PraosNonce)
neutralNonceParser =
  string "neutral" >> pure Nothing
