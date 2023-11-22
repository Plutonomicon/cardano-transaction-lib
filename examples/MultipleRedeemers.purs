module Ctl.Examples.MultipleRedeemers
  ( contract
  , contractWithMintRedeemers
  , redeemerIs1Validator
  , redeemerIs2Validator
  , redeemerIs3Validator
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad (Contract)
import Contract.PlutusData
  ( PlutusData(Integer)
  , Redeemer(Redeemer)
  , toData
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( Validator(Validator)
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV1FromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Helpers
  ( mkCurrencySymbol
  , mkTokenName
  )
import Ctl.Examples.MintsMultipleTokens
  ( mintingPolicyRdmrInt3
  )
import Ctl.Examples.PlutusV2.ReferenceInputsAndScripts
  ( mintAlwaysMintsV2ToTheScript
  )
import Ctl.Examples.PlutusV2.Scripts.AlwaysMints (alwaysMintsPolicyV2)
import Data.List as List
import Data.Map as Map
import Data.Traversable (sequence)
import Effect.Exception (error)
import JS.BigInt as BigInt

contract :: Contract Unit
contract = do
  tokenName <- mkTokenName "Token"
  validator1 <- redeemerIs1Validator
  validator2 <- redeemerIs2Validator
  mintingPolicy <- alwaysMintsPolicyV2

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
    (Lookups.mintingPolicy mintingPolicy <> lookups)
    constraints
  void $ awaitTxConfirmed txHash

contractWithMintRedeemers :: Contract Unit
contractWithMintRedeemers = do
  tokenName <- mkTokenName "Token"
  validator1 <- redeemerIs1Validator
  mintingPolicy <- alwaysMintsPolicyV2
  mp /\ cs <- mkCurrencySymbol mintingPolicyRdmrInt3

  -- Lock tokens on script address

  _ <- mintAlwaysMintsV2ToTheScript tokenName validator1 100

  --- Unlock tokens and mint other tokens with redeemer

  lcs <- spendLockedByIntOutputParams (validator1 /\ 1)
  let unlockingLookups = snd lcs :: Lookups.ScriptLookups
  let unlockingConstraints = fst lcs :: Constraints.TxConstraints
  let
    mintingConstraints =
      ( Constraints.mustMintValueWithRedeemer
          (Redeemer $ Integer $ BigInt.fromInt 3)
          (Value.singleton cs tokenName one)
      )
  txHash <- submitTxFromConstraints
    ( Lookups.mintingPolicy mintingPolicy <> Lookups.mintingPolicy mp <>
        unlockingLookups
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
  utxo <- utxosAt (scriptHashAddress vhash Nothing)
  constraints <- pure $ mconcat do
    input <- List.fromFoldable $ Map.keys utxo
    pure $
      ( Constraints.mustSpendScriptOutput input
          $ Redeemer
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
    Validator <$> plutusScriptV1FromEnvelope envelope

-- | checks whether redeemer is 2
redeemerIs2Validator :: Contract Validator
redeemerIs2Validator = do
  liftMaybe (error "Error decoding redeemerIs2Script") do
    envelope <- decodeTextEnvelope redeemerIs2Script
    Validator <$> plutusScriptV1FromEnvelope envelope

-- | checks whether redeemer is 3
redeemerIs3Validator :: Contract Validator
redeemerIs3Validator = do
  liftMaybe (error "Error decoding redeemerIs3Script") do
    envelope <- decodeTextEnvelope redeemerIs3Script
    Validator <$> plutusScriptV1FromEnvelope envelope
