module Ctl.Examples.MultipleRedeemers (contract, main) where

import Contract.Prelude

import Aeson (decodeAeson, fromString)
import Contract.Address (ownPaymentPubKeyHash, PaymentPubKeyHash)
import Contract.Config (ConfigParams, testnetConfig, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, ContractEnv(..), launchAff_, liftContractM, liftedE, liftedM, runContract)
import Contract.PlutusData (Redeemer(Redeemer), PlutusData(Integer), toData, unitDatum)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, MintingPolicy, ValidatorHash, PlutusScript, scriptHashAddress, validatorHash)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Transaction (TransactionHash, TransactionInput, balanceAndSignTx, submit)
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (CurrencySymbol, TokenName, Value, mkTokenName, scriptCurrencySymbol)
import Contract.Value as Value
import Control.Monad.Reader.Trans (ask)
import Data.Array (replicate)
import Data.BigInt (fromInt)
import Data.Bifunctor(lmap)
import Data.Bitraversable (ltraverse)
import Data.Foldable (length, sum)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Set as Set
import Effect.Aff (delay, Milliseconds(Milliseconds))

-- | to run this, edit `ps-entrypoint` in the MakeFile
main :: Effect Unit
main = example testnetNamiConfig

type Configuration =
  ( -- | the scripts we're going to lock the utxos at 
    validators :: Array (Validator /\ Redeemer)
  -- | the Tokennames and the amounts we're going to luck
  , tokens :: Array (String /\ Int)
  -- | the CurrencySymbols wee're gonig to look the tokens at 
  , policies :: Array (MintingPolicy /\ Redeemer)
  )

-- Obtain minting policies and validators and generate the configuration
-- for the contracts
initialise :: Contract () (ConfigParams Configuration)
initialise = do
  _ :: PlutusScript <- liftedE <<< pure <<< decodeAeson <<< fromString $ "5601000022325333573466e1d2002001149858dd680101"
  (mp1' /\ mp2' /\ mp3') <- do
    m1 <- liftContractM "Could not obtain MintingPolicy for Redeemer 1" mp1
    m2 <- liftContractM "Could not obtain MintingPolicy for Redeemer 2" mp2
    m3 <- liftContractM "Could not obtain MintingPolicy for Redeemer 3" mp3
    pure $ m1 /\ m2 /\ m3

  (red1 /\ red2 /\ red3) <- do
    r1 <- liftContractM "Could not obtain Validator for 1" isRedeemedBy1Script
    r2 <- liftContractM "Could not obtain Validator for 2" isRedeemedBy2Script
    r3 <- liftContractM "Could not obtain Validator for 3" isRedeemedBy3Script
    pure $ r1 /\ r2 /\ r3

  let
    configuration :: Record Configuration
    configuration =
      { validators:
          [ red1 /\ Redeemer (toData $ Integer $ fromInt 1)
          , red2 /\ Redeemer (toData $ Integer $ fromInt 2)
          , red3 /\ Redeemer (toData $ Integer $ fromInt 3)
          ]
      , tokens:
          [ Tuple "foo" 3
          , Tuple "bar" 5
          ]
      , policies:
          [ mp1' /\ Redeemer (toData $ Integer $ fromInt 1)
          , mp2' /\ Redeemer (toData $ Integer $ fromInt 2)
          , mp3' /\ Redeemer (toData $ Integer $ fromInt 3)
          ]
      }

  pure $ testnetConfig { extraConfig = configuration }

-- FIXME: this doesn't work without a browser
contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.MultipleRedeemers"
  cfg :: ConfigParams Configuration <- initialise

  liftAff <<< runContract cfg $ do
      hash <- createTokens
      logInfo' $ "Created utxos with transactionhash " <> show hash
      logInfo' "Going on with spending scriptoutputs from previous transaction"
      spendTokens hash

-- | At each script we lock n of each tokens, contained in single utxos 
-- | For each of the CurrencySymbols we mint a value with a correspnding redeemer
createTokens
  :: Contract Configuration TransactionHash
createTokens = do
  ContractEnv {
    extraConfig:
      { tokens
      , policies
      , validators
      }
  } <- ask

  css :: Array (CurrencySymbol /\ Redeemer) <-
    liftContractM "Could not obtain currency symbol from minting policy" $
      sequence $ ltraverse scriptCurrencySymbol <$> policies

  toks :: Array (Tuple TokenName Int) <-
    liftContractM "Could not obtain TokenName" $
      sequence $ ltraverse (mkTokenName <=< byteArrayFromAscii) <$> tokens

  let
    toCsValue :: Array (Tuple TokenName Int) -> CurrencySymbol -> Value
    toCsValue t cs = mconcat $ map (\(tn /\ n) -> Value.singleton cs tn $ fromInt n) t

    values :: Array (Value /\ Redeemer)
    values = lmap (toCsValue toks) <$> css

    vhashes :: Array ValidatorHash
    vhashes = validatorHash <<< fst <$> validators

  logInfo' $ "Trying to create " <> show values


  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mconcat
      [ mconcat $ Lookups.validator <<< fst <$> validators
      , mconcat $ Lookups.mintingPolicy <<< fst <$> policies
      ]

    constraints :: Constraints.TxConstraints Unit Unit
    constraints = mconcat
      [ mconcat $ do
          val /\ red <- values
          pure $ Constraints.mustMintValueWithRedeemer red val
      -- There will be `amount` UTxOs for every `CurrencySymbol`, `TokenName` and
      -- `Validator. Every UTxO will contain `amount` tokens.
      , mconcat $ do
          vhash <- vhashes
          (cs /\ _) <- css
          (tok /\ amount) <- toks
          replicate amount
            $ Constraints.mustPayToScript vhash unitDatum DatumWitness
            $ flip toCsValue cs
            $ pure
            $ Tuple tok 1
      , Constraints.mustIncludeDatum unitDatum
      ]

  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <-
    liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx

  logInfo' $ "Balanced and signed tx is " <> show bsTx

  submit bsTx

-- | for each Script we have one redeemer that we're goig to supply
-- | for each MintingPolicy we spend the specified count of tokens with the names specified
-- | but one token each. We do not add datums, the redeemers are tupled up with the Scripts
spendTokens
  :: TransactionHash -> Contract Configuration Unit
spendTokens hash = do
  ContractEnv {
    extraConfig:
      { validators
      , policies
      }
  } <- ask

  utxosnreds :: Array (UtxoMap /\ Redeemer) <- getUtxos hash

  self :: PaymentPubKeyHash <- liftedM "Could not obtain own PaymentPubKeyHash"
    ownPaymentPubKeyHash

  logInfo' $ "Found " <> show (getorefs hash <<< fst <$> utxosnreds) <>
    " at alwaysSucceeeds address"

  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mconcat
      [ mconcat $ Lookups.unspentOutputs <<< fst <$> utxosnreds
      , mconcat $ Lookups.validator <<< fst <$> validators
      , mconcat $ Lookups.mintingPolicy <<< fst <$> policies
      , Lookups.ownPaymentPubKeyHash self
      ]

    constraints :: Constraints.TxConstraints Unit Unit
    constraints = mconcat $
      [ mconcat $ do
          (utxo /\ red) <- utxosnreds
          flip Constraints.mustSpendScriptOutput red <$> getorefs hash utxo
      ]

  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <-
    liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  hash2 <- submit bsTx
  logInfo' $ "Hash of second transaction " <> show hash2
  pure unit

getUtxos
  :: forall (r :: Row Type)
   . TransactionHash
  -> Contract Configuration (Array (UtxoMap /\ Redeemer))
getUtxos hash = go
  where
  go = do
    ContractEnv ({
      extraConfig:
        { tokens
        , policies
        , validators
        }
      }) <- ask

    utxos :: Array (UtxoMap /\ Redeemer) <- for validators $ \(Tuple val red) ->
      do
        let vhash = validatorHash val
        utxo <- liftContractM ("could not get utxos at " <> show vhash) =<<
          utxosAt
            (scriptHashAddress vhash)
        pure $ utxo /\ red

    liftAff $ delay $ Milliseconds $ toNumber 3_000
    let
      orefs :: Array (Array TransactionInput)
      orefs = getorefs hash <<< fst <$> utxos

      -- for each of the tokensets, for each of the validators, for each of the policies we get one token
      tokenCount :: Int
      tokenCount = (sum $ snd <$> tokens) * length validators * length policies

      utxoCount :: Int
      utxoCount = sum (length <$> orefs)

    logInfo' $ "Searching for " <> show tokenCount <> " tokens"

    if (utxoCount == tokenCount) then do
      logInfo' $ "after filtering there are " <> show utxoCount <> " utxos"
      pure utxos
    else do
      logInfo' "Could not find utxos, trying again"
      logInfo' $ "after filtering there are " <> show utxoCount <> " utxos"
      go

getorefs :: TransactionHash -> UtxoMap -> Array TransactionInput
getorefs hash utxo = Set.toUnfoldable
  $ Set.filter ((_ == hash) <<< _.transactionId <<< unwrap)
  $ Map.keys utxo

-- | checks whether redeemer is 1
isRedeemedBy1Script :: Maybe Validator
isRedeemedBy1Script = map wrap $ hush $ decodeAeson $ fromString
  "56010000222325333573466e1c0052002149858dd68011"

-- | checks whether redeemer is 2
isRedeemedBy2Script :: Maybe Validator
isRedeemedBy2Script = mkScript
  "56010000222325333573466e1c0052004149858dd68011"

-- | checks whether redeemer is 3
isRedeemedBy3Script :: Maybe Validator
isRedeemedBy3Script = mkScript
  "56010000222325333573466e1c0052006149858dd68011"

mkScript :: forall b. Newtype b PlutusScript => String -> Maybe b
mkScript = map wrap <<< hush <<< decodeAeson <<< fromString

-- | checks whether redeemer is 1
mp1 :: Maybe MintingPolicy
mp1 = mkScript
  "5601000022325333573466e1d2002001149858dd680101"

-- | checks whether redeemer is 2
mp2 :: Maybe MintingPolicy
mp2 = mkScript
  "5601000022325333573466e1d2004001149858dd680101"

-- | checks whether redeemer is 3
mp3 :: Maybe MintingPolicy
mp3 = mkScript
  "5601000022325333573466e1d2006001149858dd680101"

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true
