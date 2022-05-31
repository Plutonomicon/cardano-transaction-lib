module Test.MultipleRedeemers (suite) where

import Prelude

import Contract.Aeson (decodeAeson, fromString)
import Contract.Monad
  ( ContractConfig(ContractConfig)
  , liftedE
  , liftedM
  , liftContractM
  , runContract
  , traceContractConfig
  )
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator(Validator), validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (mkNamiWalletAff)
import Data.BigInt (fromInt)
import Data.Either (hush)
import Contract.Prelude (mconcat)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Mote (test)
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite = do
  test "contract with three redeemers" threeRedeemerContract

threeRedeemerContract :: Aff Unit
threeRedeemerContract = do
  ContractConfig defaults <- traceContractConfig
  -- how do we get a wallet?
  wallet <- mkNamiWalletAff
  runContract (ContractConfig $ defaults { wallet = pure wallet }) $ do
    validator <- liftContractM "Invalid script JSON" $ alwaysSucceedsScript
    vhash <- liftContractM "could not hash validator" $ validatorHash validator
    let
      constraints :: Constraints.TxConstraints Unit Unit
      constraints = mconcat $
        Constraints.mustPayToScript vhash unitDatum <<< lovelaceValueOf <<<
          fromInt <$> [ 2_000_000, 3_000_000, 4_000_000 ]

      lookups :: Lookups.ScriptLookups PlutusData
      lookups = Lookups.validator validator
    ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    BalancedSignedTransaction bsTx <-
      liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
    submit bsTx.signedTxCbor *> pure unit
  pure unit

alwaysSucceedsScript :: Maybe Validator
alwaysSucceedsScript = map Validator $ hush $ decodeAeson $ fromString
  "4d01000033222220051200120011"
