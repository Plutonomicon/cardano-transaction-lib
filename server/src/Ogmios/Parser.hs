module Ogmios.Parser (decodeProtocolParameters) where

import Cardano.Api (
  AnyPlutusScriptVersion (AnyPlutusScriptVersion),
  CostModel,
  ExecutionUnitPrices (ExecutionUnitPrices),
  PlutusScriptVersion (PlutusScriptV1, PlutusScriptV2),
 )
import Cardano.Api.Shelley (ProtocolParameters (ProtocolParameters))
import Data.Aeson (FromJSON (parseJSON), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ratio ((%))
import Data.Text (Text)
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
          <*> pure 0 -- `decentralizationParameter` is deprecated
          <*> pure Nothing -- `extraEntropy` is deprecated
          <*> o .: "maxBlockHeaderSize"
          <*> o .: "maxBlockBodySize"
          <*> o .: "maxTxSize"
          <*> o .: "minFeeConstant"
          <*> o .: "minFeeCoefficient"
          <*> pure Nothing -- `minUtxO` is deprecated
          <*> o .: "stakeKeyDeposit"
          <*> o .: "poolDeposit"
          <*> o .: "minPoolCost"
          <*> o .: "poolRetirementEpochBound"
          <*> o .: "desiredNumberOfPools"
          <*> (rationalP =<< o .: "poolInfluence")
          <*> (rationalP =<< o .: "monetaryExpansion")
          <*> (rationalP =<< o .: "treasuryExpansion")
          <*> (fmap (* 8) <$> o .:? "coinsPerUtxoByte")
          <*> ( maybe (pure Map.empty) parseCostmodels
                  =<< (o .:? "costModels")
              )
          <*> parseExecutionPrices o
          <*> o .:? "maxExecutionUnitsPerTransaction"
          <*> o .:? "maxExecutionUnitsPerBlock"
          <*> o .:? "maxValueSize"
          <*> o .:? "collateralPercentage"
          <*> o .:? "maxCollateralInputs"
      pure $ ProtocolParametersWrapper params

parseCostmodels ::
  Map Text Aeson.Types.Object ->
  Aeson.Types.Parser (Map AnyPlutusScriptVersion CostModel)
parseCostmodels = fmap Map.fromList . traverse parseModel . Map.toList
  where
    parseModel ::
      (Text, Aeson.Types.Object) ->
      Aeson.Types.Parser (AnyPlutusScriptVersion, CostModel)
    parseModel (k, o) =
      (,) <$> parseScriptVersion k
        <*> parseJSON (Aeson.Types.Object o)

parseScriptVersion :: Text -> Aeson.Types.Parser AnyPlutusScriptVersion
parseScriptVersion = \case
  "plutus:v1" -> pure $ AnyPlutusScriptVersion PlutusScriptV1
  "plutus:v2" -> pure $ AnyPlutusScriptVersion PlutusScriptV2
  _ -> fail "Unrecognized Plutus script tag"

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
(ogmios : "neutral) making the hex parser from Aeson to fail. From code we can
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
decodeProtocolParameters = fmap unwrapParams . Aeson.eitherDecode
