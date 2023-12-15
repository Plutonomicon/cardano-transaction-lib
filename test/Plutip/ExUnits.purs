module Test.Ctl.Plutip.ExUnits
  ( mkSuite
  , mkFailingSuite
  ) where

import Prelude

import Contract.Log (logInfo')
import Contract.Scripts (validatorHash)
import Contract.Test (ContractTest, InitialUTxOs, withKeyWallet, withWallets)
import Contract.Transaction (awaitTxConfirmed)
import Ctl.Examples.ExUnits as ExUnits
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Either (isLeft)
import Effect.Aff (try)
import JS.BigInt as BigInt
import Mote (test)
import Test.Spec.Assertions (shouldSatisfy)

mkSuite :: Int -> TestPlanM ContractTest Unit
mkSuite n = do
  test ("ExUnits.plutus script with " <> show n <> " iterations - must succeed")
    do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 500_000_000
          , BigInt.fromInt 500_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          validator <- ExUnits.exUnitsScript
          let vhash = validatorHash validator
          logInfo' "Attempt to lock value"
          txId <- ExUnits.payToExUnits vhash
          awaitTxConfirmed txId
          logInfo' "Try to spend locked values"
          ExUnits.spendFromExUnits (BigInt.fromInt n) vhash validator txId

mkFailingSuite :: Int -> TestPlanM ContractTest Unit
mkFailingSuite n = do
  test ("ExUnits.plutus script with " <> show n <> " iterations - must fail")
    do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 500_000_000
          , BigInt.fromInt 500_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          validator <- ExUnits.exUnitsScript
          let vhash = validatorHash validator
          logInfo' "Attempt to lock value"
          txId <- ExUnits.payToExUnits vhash
          awaitTxConfirmed txId
          logInfo' "Try to spend locked values"
          res <- try $ ExUnits.spendFromExUnits (BigInt.fromInt n) vhash
            validator
            txId
          res `shouldSatisfy` isLeft
