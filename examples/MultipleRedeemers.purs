module Ctl.Examples.MultipleRedeemers
  ( contract
  , contractWithMintRedeemers
  , redeemerIs1Validator
  , redeemerIs2Validator
  , redeemerIs3Validator
  ) where

import Contract.Prelude

import Cardano.Types (AssetName, Credential(ScriptHashCredential))
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.Value as Value
import Contract.Address (mkAddress)
import Contract.Monad (Contract)
import Contract.PlutusData
  ( PlutusData(Integer)
  , RedeemerDatum(RedeemerDatum)
  , toData
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Helpers (mkAssetName)
import Ctl.Examples.PlutusV2.Scripts.AlwaysMints (alwaysMintsPolicyScriptV2)
import Data.List as List
import Data.Map as Map
import Data.Traversable (sequence)
import Effect.Exception (error)
import JS.BigInt as BigInt

contract :: Contract Unit
contract = do
  tokenName <- mkAssetName "Token"
  validator1 <- redeemerIs1Validator
  validator2 <- redeemerIs2Validator
  mintingPolicy <- alwaysMintsPolicyScriptV2

  -- Lock tokens on different script addresses

  _ <- mintAlwaysMintsV2ToTheScript tokenName validator1 100
  _ <- mintAlwaysMintsV2ToTheScript tokenName validator2 100

  --- Unlock all those tokens with redeemer

  lcs <- sequence $ spendLockedByIntOutputParams <$>
    [ (validator1 /\ 1), (validator2 /\ 2) ]
  let lookups = (mconcat $ snd <$> lcs) :: Lookups.ScriptLookups
  let
    constraints =
      (mconcat $ fst <$> lcs) :: Constraints.TxConstraints
  txHash <- submitTxFromConstraints
    (Lookups.plutusMintingPolicy mintingPolicy <> lookups)
    constraints
  void $ awaitTxConfirmed txHash

contractWithMintRedeemers :: Contract Unit
contractWithMintRedeemers = do
  tokenName <- mkAssetName "Token"
  validator1 <- redeemerIs1Validator
  mintingPolicy <- alwaysMintsPolicyScriptV2
  let cs = PlutusScript.hash mintingPolicy

  -- Lock tokens on script address

  _ <- mintAlwaysMintsV2ToTheScript tokenName validator1 100

  --- Unlock tokens and mint other tokens with redeemer

  lcs <- spendLockedByIntOutputParams (validator1 /\ 1)
  let unlockingLookups = snd lcs :: Lookups.ScriptLookups
  let unlockingConstraints = fst lcs :: Constraints.TxConstraints
  let
    mintingConstraints =
      ( Constraints.mustMintValueWithRedeemer
          (RedeemerDatum $ Integer $ BigInt.fromInt 3)
          (Mint.singleton cs tokenName Int.one)
      )
  txHash <- submitTxFromConstraints
    ( Lookups.plutusMintingPolicy mintingPolicy
        <> Lookups.plutusMintingPolicy mintingPolicy
        <> unlockingLookups
    )
    (unlockingConstraints <> mintingConstraints)
  void $ awaitTxConfirmed txHash

spendLockedByIntOutputParams
  :: Tuple Validator Int
  -> Contract
       ( Tuple (Constraints.TxConstraints)
           (Lookups.ScriptLookups)
       )
spendLockedByIntOutputParams (validator /\ redeemerVal) = do
  let vhash = validatorHash validator
  utxo <- utxosAt =<< mkAddress (wrap $ ScriptHashCredential vhash) Nothing
  constraints <- pure $ mconcat do
    input <- List.fromFoldable $ Map.keys utxo
    pure $
      ( Constraints.mustSpendScriptOutput input
          $ RedeemerDatum
          $ toData
          $ Integer
          $ BigInt.fromInt redeemerVal
      )
  pure $ constraints /\
    (Lookups.unspentOutputs utxo <> Lookups.validator validator)

foreign import redeemerIs1Script :: String
foreign import redeemerIs2Script :: String
foreign import redeemerIs3Script :: String

-- | checks whether redeemer is 1
redeemerIs1Validator :: Contract Validator
redeemerIs1Validator = do
  liftMaybe (error "Error decoding redeemerIs1Script") do
    envelope <- decodeTextEnvelope redeemerIs1Script
    plutusScriptFromEnvelope envelope

-- | checks whether redeemer is 2
redeemerIs2Validator :: Contract Validator
redeemerIs2Validator = do
  liftMaybe (error "Error decoding redeemerIs2Script") do
    envelope <- decodeTextEnvelope redeemerIs2Script
    plutusScriptFromEnvelope envelope

-- | checks whether redeemer is 3
redeemerIs3Validator :: Contract Validator
redeemerIs3Validator = do
  liftMaybe (error "Error decoding redeemerIs3Script") do
    envelope <- decodeTextEnvelope redeemerIs3Script
    plutusScriptFromEnvelope envelope

mintAlwaysMintsV2ToTheScript
  :: AssetName -> Validator -> Int -> Contract Unit
mintAlwaysMintsV2ToTheScript tokenName validator sum = do
  mp <- alwaysMintsPolicyScriptV2
  let cs = PlutusScript.hash mp

  let
    vhash = PlutusScript.hash validator

    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue
          $ Mint.singleton cs tokenName
          $ Int.fromInt sum
      , Constraints.mustPayToScript vhash PlutusData.unit
          Constraints.DatumWitness
          $ Value.singleton cs tokenName
          $ BigNum.fromInt sum
      ]

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.plutusMintingPolicy mp

  txHash <- submitTxFromConstraints lookups constraints
  void $ awaitTxConfirmed txHash
