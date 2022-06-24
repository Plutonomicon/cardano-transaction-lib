-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a value using three minting policies with different redeemers.
module Examples.MintsMultipleTokens (main) where

import Contract.Prelude

import Contract.Aeson (decodeAeson, fromString)
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
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
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
    BalancedSignedTransaction bsTx <-
      liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
    txId <- submit bsTx.signedTxCbor
    logInfo' $ "Tx ID: " <> show txId

mkTokenName :: forall (r :: Row Type). String -> Contract r TokenName
mkTokenName =
  liftContractM "Cannot make token name"
    <<< (Value.mkTokenName <=< byteArrayFromAscii)

mkCurrencySymbol
  :: forall (r :: Row Type)
   . Maybe MintingPolicy
  -> Contract r (MintingPolicy /\ CurrencySymbol)
mkCurrencySymbol mintingPolicy = do
  mp <- liftContractM "Invalid script JSON" mintingPolicy
  cs <- liftContractAffM "Cannot get cs" $ Value.scriptCurrencySymbol mp
  pure (mp /\ cs)

foreign import mintingPolicyRdmrInt1Cbor :: String
foreign import mintingPolicyRdmrInt2Cbor :: String
foreign import mintingPolicyRdmrInt3Cbor :: String

mintingPolicyRdmrInt1 :: Maybe MintingPolicy
mintingPolicyRdmrInt1 = map wrap $ hush $ decodeAeson $ fromString
  mintingPolicyRdmrInt1Cbor

mintingPolicyRdmrInt2 :: Maybe MintingPolicy
mintingPolicyRdmrInt2 = map wrap $ hush $ decodeAeson $ fromString
  mintingPolicyRdmrInt2Cbor

mintingPolicyRdmrInt3 :: Maybe MintingPolicy
mintingPolicyRdmrInt3 = map wrap $ hush $ decodeAeson $ fromString
  mintingPolicyRdmrInt3Cbor
