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
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text qualified as Text
import Text.Read (readMaybe)

newtype ProtocolParametersWrapper = ProtocolParametersWrapper
  { unwrapParams :: ProtocolParameters
  }

instance Aeson.FromJSON ProtocolParametersWrapper where
  parseJSON =
    Aeson.withObject "ProtocolParametersWrapper" $ \top -> do
      o <- top .: "result"
      v <- o .: "protocolVersion"
      params <-
        ProtocolParameters
          <$> ((,) <$> v .: "major" <*> v .: "minor")
          <*> (rationalP =<< o .: "decentralizationParameter")
          <*> (o .: "extraEntropy" <|> (neutralNonceP =<< o .: "extraEntropy"))
          <*> o .: "maxBlockHeaderSize"
          <*> o .: "maxBlockBodySize"
          <*> o .: "maxTxSize"
          <*> o .: "minFeeConstant"
          <*> o .: "minFeeCoefficient"
          <*> o .:? "minUtxoValue"
          <*> o .: "stakeKeyDeposit"
          <*> o .: "poolDeposit"
          <*> o .: "minPoolCost"
          <*> o .: "poolRetirementEpochBound"
          <*> o .: "desiredNumberOfPools"
          <*> (rationalP =<< o .: "poolInfluence")
          <*> (rationalP =<< o .: "monetaryExpansion")
          <*> (rationalP =<< o .: "treasuryExpansion")
          <*> o .:? "coinsPerUtxoWord"
          <*> (o .:? "costModels" .!= Map.empty)
          <*> parseExecutionPrices o
          <*> o .:? "maxExecutionUnitsPerTransaction"
          <*> o .:? "maxExecutionUnitsPerBlock"
          <*> o .:? "maxValueSize"
          <*> o .:? "collateralPercentage"
          <*> o .:? "maxCollateralInputs"
      pure $ ProtocolParametersWrapper params

modifyPlutusName :: Aeson.Value -> Aeson.Value
modifyPlutusName originalValue@(Aeson.Object keyHashMap) =
  fromMaybe originalValue fixedName
  where
    fixedName :: Maybe Aeson.Value
    fixedName = do
      result <- HashMap.lookup "result" keyHashMap >>= unwrapObject
      costModels <- HashMap.lookup "costModels" result >>= unwrapObject
      plutus <- HashMap.lookup "plutus:v1" costModels
      let newCostModel =
            HashMap.insert "PlutusScriptV1" plutus $
              HashMap.delete "plutus:v1" costModels
          newResult =
            HashMap.insert "costModels" (Aeson.Object newCostModel) result
          newKeyHashMap =
            HashMap.insert "result" (Aeson.Object newResult) keyHashMap
      pure $ Aeson.Object newKeyHashMap

    unwrapObject :: Aeson.Value -> Maybe Aeson.Object
    unwrapObject (Aeson.Object obj) = Just obj
    unwrapObject _ = Nothing
modifyPlutusName x = x

parseExecutionPrices ::
  Aeson.Object -> Aeson.Types.Parser (Maybe ExecutionUnitPrices)
parseExecutionPrices obj =
  obj .:? "prices" >>= \case
    Just v ->
      Just
        <$> ( ExecutionUnitPrices
                <$> (rationalP =<< v .: "steps")
                <*> (rationalP =<< v .: "memory")
            )
    _ -> pure Nothing

{- |
`extraEntropy` has been added as part of the cardano decentralization
https://iohk.io/en/blog/posts/2021/03/29/the-secure-transition-to-decentralization/
(more details on cip 9)

It must be a Hex encoded string, but currently is set as "neutralNonce"
(ogmios : "neutral") making the hex parser from Aeson to fail. From code we can
see that currently a @Nothing@ value is translated to @Ledger.NeutralNonce@
-}
rationalP :: Aeson.Value -> Aeson.Types.Parser Rational
rationalP = Aeson.withText "Rational" $ \t ->
  case Text.unpack <$> Text.split (== '/') t of
    [n, d] -> maybe parseFail pure $ (%) <$> readMaybe n <*> readMaybe d
    _ -> parseFail
  where
    parseFail :: Aeson.Types.Parser Rational
    parseFail = fail "Couldn't parse `Rational` value"

decodeProtocolParameters :: ByteString -> Either String ProtocolParameters
decodeProtocolParameters response =
  case modifyPlutusName <$> Aeson.decode response of
    Just modifiedResponse ->
      unwrapParams
        <$> Aeson.eitherDecode @ProtocolParametersWrapper
          (Aeson.encode modifiedResponse)
    _ -> Left "Can't modify costModels name in response"

neutralNonceP :: Aeson.Value -> Aeson.Types.Parser (Maybe PraosNonce)
neutralNonceP = Aeson.withText "(Maybe PraosNonce)" $ \case
  "neutral" -> pure Nothing
  _ -> fail "Couldn't parse `(Maybe PraosNonce)`"
