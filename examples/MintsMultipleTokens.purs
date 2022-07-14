-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a value using three minting policies with different redeemers.
module Examples.MintsMultipleTokens (main) where

import Contract.Prelude

import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractAffM
  , liftContractM
  , liftedE
  , liftedM
  , logInfo'
  , runContract_
  , traceTestnetContractConfig
  )
import Contract.PlutusData (PlutusData(Integer), Redeemer(Redeemer))
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import Contract.Transaction (balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Data.BigInt (fromInt) as BigInt

main :: Effect Unit
main = launchAff_ $ do
  cfg <- traceTestnetContractConfig
  runContract_ cfg $ do
    logInfo' "Running Examples.MintsMultipleTokens"
    tn1 <- mkTokenName "Token with a long name"
    tn2 <- mkTokenName "Token"
    mp1 /\ cs1 <- mkCurrencySymbol mintingPolicyRdmrInt1
    mp2 /\ cs2 <- mkCurrencySymbol mintingPolicyRdmrInt2
    mp3 /\ cs3 <- mkCurrencySymbol mintingPolicyRdmrInt3

    let
      constraints :: Constraints.TxConstraints Void Void
      constraints = mconcat
        [ Constraints.mustMintValueWithRedeemer
            (Redeemer $ Integer (BigInt.fromInt 1))
            (Value.singleton cs1 tn1 one <> Value.singleton cs1 tn2 one)
        , Constraints.mustMintValueWithRedeemer
            (Redeemer $ Integer (BigInt.fromInt 2))
            (Value.singleton cs2 tn1 one <> Value.singleton cs2 tn2 one)
        , Constraints.mustMintValueWithRedeemer
            (Redeemer $ Integer (BigInt.fromInt 3))
            (Value.singleton cs3 tn1 one <> Value.singleton cs3 tn2 one)
        ]

      lookups :: Lookups.ScriptLookups Void
      lookups =
        Lookups.mintingPolicy mp1
          <> Lookups.mintingPolicy mp2
          <> Lookups.mintingPolicy mp3

    ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    bsTx <-
      liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
    txId <- submit bsTx
    logInfo' $ "Tx ID: " <> show txId

mkTokenName :: forall (r :: Row Type). String -> Contract r TokenName
mkTokenName =
  liftContractM "Cannot make token name"
    <<< (Value.mkTokenName <=< byteArrayFromAscii)

mkCurrencySymbol
  :: forall (r :: Row Type)
   . Contract r MintingPolicy
  -> Contract r (MintingPolicy /\ CurrencySymbol)
mkCurrencySymbol mintingPolicy = do
  mp <- mintingPolicy
  cs <- liftContractAffM "Cannot get cs" $ Value.scriptCurrencySymbol mp
  pure (mp /\ cs)

foreign import redeemerInt1 :: String
foreign import redeemerInt2 :: String
foreign import redeemerInt3 :: String

mintingPolicyRdmrInt1 :: forall r. Contract r MintingPolicy
mintingPolicyRdmrInt1 = wrap <<< wrap <$> textEnvelopeBytes redeemerInt1
  PlutusScriptV1

mintingPolicyRdmrInt2 :: forall r. Contract r MintingPolicy
mintingPolicyRdmrInt2 = wrap <<< wrap <$> textEnvelopeBytes redeemerInt2
  PlutusScriptV1

mintingPolicyRdmrInt3 :: forall r. Contract r MintingPolicy
mintingPolicyRdmrInt3 = wrap <<< wrap <$> textEnvelopeBytes redeemerInt3
  PlutusScriptV1
