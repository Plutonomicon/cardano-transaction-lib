module Ctl.Examples.MultipleRedeemers (contract, main) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash, PaymentPubKeyHash)
import Contract.Config (ConfigParams, testnetConfig, testnetNamiConfig)
import Contract.Log (logInfo', logDebug')
import Contract.Monad (Contract, ContractEnv(..), launchAff_, liftContractM, liftedE, liftedM, runContract)
import Contract.PlutusData (Redeemer(Redeemer), PlutusData(Integer), toData, unitDatum)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, MintingPolicy, ValidatorHash, PlutusScript, scriptHashAddress, validatorHash)
import Contract.Test.E2E (publishTestFeedback)
import Contract.TextEnvelope (textEnvelopeBytes, TextEnvelopeType(PlutusScriptV1))
import Contract.Transaction (TransactionHash, TransactionInput, balanceAndSignTx, balanceAndSignTxE, plutusV1Script, submit)
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (CurrencySymbol, TokenName, Value, mkTokenName, scriptCurrencySymbol)
import Contract.Value as Value
import Control.Monad.Reader.Trans (ask)
import Data.Array (replicate)
import Data.Bifunctor (lmap)
import Data.BigInt (fromInt)
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
  { -- | the scripts we're going to lock the utxos at
    validators :: Array (Validator /\ Redeemer)
  -- | the Tokennames and the amounts we're going to luck
  , tokens :: Array (String /\ Int)
  -- | the CurrencySymbols wee're gonig to look the tokens at 
  , policies :: Array (MintingPolicy /\ Redeemer)
  }

-- Obtain minting policies and validators and generate the configuration
-- for the contracts
initialise :: Contract () Configuration
initialise = do
  (mp1' /\ mp2' /\ mp3') <- do
    m1 <- mp1
    m2 <- mp2
    m3 <- mp3
    pure $ m1 /\ m2 /\ m3

  (red1 /\ red2 /\ red3) <- do
    r1 <- val1
    r2 <- val2
    r3 <- val3
    pure $ r1 /\ r2 /\ r3

  let
    configuration :: Configuration
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

  pure configuration

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.MultipleRedeemers"
  cfg :: Configuration <- initialise
  logInfo' "Initialise succeeded"

  hash <- createTokens cfg
  logInfo' $ "Created utxos with transactionhash " <> show hash
  logInfo' "Going on with spending scriptoutputs from previous transaction"
  spendTokens cfg hash

-- | At each script we lock n of each tokens, contained in single utxos 
-- | For each of the CurrencySymbols we mint a value with a correspnding redeemer
createTokens
  :: Configuration -> Contract () TransactionHash
createTokens {tokens, policies, validators} = do
  logInfo' "createToken start"

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
          _ <- vhashes
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

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  balancedSignedTx <- liftedE $ balanceAndSignTxE unbalancedTx
  logInfo' $ "Balanced and signed tx is " <> show balancedSignedTx
  txId <- submit balancedSignedTx
  logInfo' $ "Tx ID: " <> show txId
  logInfo' "createToken end"
  pure txId

-- | for each Script we have one redeemer that we're goig to supply
-- | for each MintingPolicy we spend the specified count of tokens with the names specified
-- | but one token each. We do not add datums, the redeemers are tupled up with the Scripts
spendTokens
  :: Configuration -> TransactionHash -> Contract () Unit
spendTokens cfg@{ validators, policies } hash = do
  utxosnreds :: Array (UtxoMap /\ Redeemer) <- getUtxos cfg hash

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
   . Configuration
  -> TransactionHash
  -> Contract () (Array (UtxoMap /\ Redeemer))
getUtxos { tokens, policies, validators } hash = go
  where
  go = do
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

mkScript :: forall b . Newtype b PlutusScript => String -> Contract () b
mkScript cbor = wrap <<< plutusV1Script <$> textEnvelopeBytes cbor PlutusScriptV1

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true

foreign import redeemerInt1 :: String
foreign import redeemerInt2 :: String
foreign import redeemerInt3 :: String
foreign import vredeemerInt1 :: String
foreign import vredeemerInt2 :: String
foreign import vredeemerInt3 :: String

-- | checks whether redeemer is 1
val1 :: Contract () Validator
val1 = mkScript vredeemerInt1

-- | checks whether redeemer is 2
val2 :: Contract () Validator
val2 = mkScript vredeemerInt2

-- | checks whether redeemer is 3
val3 :: Contract () Validator
val3 = mkScript vredeemerInt3

-- | checks whether redeemer is 1
mp1 :: Contract () MintingPolicy
mp1 = mkScript redeemerInt1

-- | checks whether redeemer is 2
mp2 :: Contract () MintingPolicy
mp2 = mkScript redeemerInt2

-- | checks whether redeemer is 3
mp3 :: Contract () MintingPolicy
mp3 = mkScript redeemerInt3

