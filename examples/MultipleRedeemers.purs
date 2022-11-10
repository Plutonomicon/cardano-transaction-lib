module Ctl.Examples.MultipleRedeemers
  ( contract
  , contractWithMintRedeemers
  , redeemerIs1Validator
  , redeemerIs2Validator
  , redeemerIs3Validator
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad (Contract, liftContractM)
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
import Contract.Transaction (awaitTxConfirmed)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Helpers
  ( buildBalanceSignAndSubmitTx
  , mkCurrencySymbol
  , mkTokenName
  )
import Ctl.Examples.MintsMultipleTokens
  ( mintingPolicyRdmrInt3
  )
import Ctl.Examples.PlutusV2.ReferenceInputs
  ( alwaysMintsPolicyV2
  , mintAlwaysMintsV2ToTheScript
  )
import Data.BigInt as BigInt
import Data.List as List
import Data.Map as Map
import Data.Traversable (sequence)
import Effect.Exception (error)

contract :: Contract () Unit
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
  let lookups = (mconcat $ snd <$> lcs) :: Lookups.ScriptLookups Void
  let
    constraints =
      (mconcat $ fst <$> lcs) :: Constraints.TxConstraints Void Void
  txHash <- buildBalanceSignAndSubmitTx
    (Lookups.mintingPolicy mintingPolicy <> lookups)
    constraints
  void $ awaitTxConfirmed txHash

contractWithMintRedeemers :: Contract () Unit
contractWithMintRedeemers = do
  tokenName <- mkTokenName "Token"
  validator1 <- redeemerIs1Validator
  mintingPolicy <- alwaysMintsPolicyV2
  mp /\ cs <- mkCurrencySymbol mintingPolicyRdmrInt3

  -- Lock tokens on script address

  _ <- mintAlwaysMintsV2ToTheScript tokenName validator1 100

  --- Unlock tokens and mint other tokens with redeemer

  lcs <- spendLockedByIntOutputParams (validator1 /\ 1)
  let unlockingLookups = snd lcs :: Lookups.ScriptLookups Void
  let unlockingConstraints = fst lcs :: Constraints.TxConstraints Void Void
  let
    mintingConstraints =
      ( Constraints.mustMintValueWithRedeemer
          (Redeemer $ Integer $ BigInt.fromInt 3)
          (Value.singleton cs tokenName one)
      )
  txHash <- buildBalanceSignAndSubmitTx
    ( Lookups.mintingPolicy mintingPolicy <> Lookups.mintingPolicy mp <>
        unlockingLookups
    )
    (unlockingConstraints <> mintingConstraints)
  void $ awaitTxConfirmed txHash

spendLockedByIntOutputParams
  :: forall x
   . Tuple Validator Int
  -> Contract x
       ( Tuple (Constraints.TxConstraints Void Void)
           (Lookups.ScriptLookups Void)
       )
spendLockedByIntOutputParams (validator /\ redeemerVal) = do
  let vhash = validatorHash validator
  utxo <- liftContractM ("could not get utxos at " <> show vhash) =<<
    utxosAt (scriptHashAddress vhash)
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

--- Importing validation scripts

foreign import vredeemerInt1 :: String
foreign import vredeemerInt2 :: String
foreign import vredeemerInt3 :: String

-- | checks whether redeemer is 1
redeemerIs1Validator :: Contract () Validator
redeemerIs1Validator = liftMaybe (error "Error decoding vredeemerInt1") do
  envelope <- decodeTextEnvelope vredeemerInt1
  Validator <$> plutusScriptV1FromEnvelope envelope

-- | checks whether redeemer is 2
redeemerIs2Validator :: Contract () Validator
redeemerIs2Validator = liftMaybe (error "Error decoding vredeemerInt2") do
  envelope <- decodeTextEnvelope vredeemerInt2
  Validator <$> plutusScriptV1FromEnvelope envelope

-- | checks whether redeemer is 3
redeemerIs3Validator :: Contract () Validator
redeemerIs3Validator = liftMaybe (error "Error decoding vredeemerInt3") do
  envelope <- decodeTextEnvelope vredeemerInt3
  Validator <$> plutusScriptV1FromEnvelope envelope
