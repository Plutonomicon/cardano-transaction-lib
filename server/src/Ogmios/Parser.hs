module Ogmios.Parser (decodeProtocolParameters) where

import Cardano.Api (
  ExecutionUnitPrices (ExecutionUnitPrices),
  PraosNonce,
 )
import Cardano.Api.Shelley (ProtocolParameters (ProtocolParameters))
import Control.Applicative ((<|>))
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.Attoparsec.Text (
  Parser,
  inClass,
  many',
  many1,
  parseOnly,
  satisfy,
  string,
 )
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text qualified as Text

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
          <*> parseRational o "decentralizationParameter"
          <*> (o .: "extraEntropy" <|> parsePraosNonceNeutral o "extraEntropy")
          <*> o .: "maxBlockHeaderSize"
          <*> o .: "maxBlockBodySize"
          <*> o .: "maxTxSize"
          <*> o .: "minFeeConstant"
          <*> o .: "minFeeCoefficient"
          -- "minUtxO is deprecated"
          <*> pure Nothing
          <*> o .: "stakeKeyDeposit"
          <*> o .: "poolDeposit"
          <*> o .: "minPoolCost"
          <*> o .: "poolRetirementEpochBound"
          <*> o .: "desiredNumberOfPools"
          <*> parseRational o "poolInfluence"
          <*> parseRational o "monetaryExpansion"
          <*> parseRational o "treasuryExpansion"
          <*> o .:? "coinsPerUtxoWord"
          <*> (o .:? "costModels" .!= Map.empty)
          <*> parseExecutionPrices o
          <*> o .:? "maxExecutionUnitsPerTransaction"
          <*> o .:? "maxExecutionUnitsPerBlock"
          <*> o .:? "maxValueSize"
          <*> o .:? "collateralPercentage"
          <*> o .:? "maxCollateralInputs"
      return $ ProtocolParametersWrapper params

modifyPlutusName :: Aeson.Value -> Aeson.Value
modifyPlutusName originalValue@(Aeson.Object keyHashMap) =
  fromMaybe originalValue fixedName
  where
    fixedName =
      do
        result <- HashMap.lookup "result" keyHashMap >>= unwrappObject
        costModels <- HashMap.lookup "costModels" result >>= unwrappObject
        plutus <- HashMap.lookup "plutus:v1" costModels
        let newCostModel =
              HashMap.insert "PlutusScriptV1" plutus (HashMap.delete "plutus:v1" costModels)
            newResult =
              HashMap.insert "costModels" (Aeson.Object newCostModel) result
            newKeyHashMap =
              HashMap.insert "result" (Aeson.Object newResult) keyHashMap
        return $ Aeson.Object newKeyHashMap

    unwrappObject :: Aeson.Value -> Maybe Aeson.Object
    unwrappObject (Aeson.Object obj) = Just obj
    unwrappObject _ = Nothing
modifyPlutusName x = x

attoparsec2Aeson :: Parser a -> Aeson.Value -> Aeson.Types.Parser a
attoparsec2Aeson p (Aeson.String str) =
  case parseOnly p str of
    Left e -> fail e
    Right x -> pure x
attoparsec2Aeson _ x = Aeson.Types.typeMismatch "Attoparsec" x

parseRational :: Aeson.Object -> Text.Text -> Aeson.Types.Parser Rational
parseRational =
  Aeson.Types.explicitParseField $
    attoparsec2Aeson rationalParser

parseExecutionPrices :: Aeson.Object -> Aeson.Types.Parser (Maybe ExecutionUnitPrices)
parseExecutionPrices obj =
  do
    maybeValue <- obj .:? "prices"
    case maybeValue of
      Just v ->
        Just
          <$> ( ExecutionUnitPrices
                  <$> parseRational v "steps"
                  <*> parseRational v "memory"
              )
      _ -> pure Nothing

--

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
parsePraosNonceNeutral :: Aeson.Object -> Text.Text -> Aeson.Types.Parser (Maybe PraosNonce)
parsePraosNonceNeutral =
  Aeson.Types.explicitParseField $
    attoparsec2Aeson neutralNonceParser

decodeProtocolParameters :: ByteString -> Either String ProtocolParameters
decodeProtocolParameters response =
  case modifyPlutusName <$> Aeson.decode response of
    Just modifiedResponse ->
      unwrapParams
        <$> Aeson.eitherDecode @ProtocolParametersWrapper
          (Aeson.encode modifiedResponse)
    _ -> Left "Can't modify costModels name in response"

nonZeroDigit :: Parser Char
nonZeroDigit = satisfy $ inClass "123456789"

digit :: Parser Char
digit = satisfy $ inClass "0123456789"

nonZeroInteger :: Parser Integer
nonZeroInteger = do
  headDigit <- nonZeroDigit
  remains <- many' digit
  pure . read $ (headDigit : remains)

zeroInteger :: Parser Integer
zeroInteger = read <$> many1 (satisfy $ inClass "0")

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
