-- | Testing assertions interface provided by `Contract.Test.Assert`
module Test.Ctl.Plutip.Contract.Assert (suite) where

import Prelude

import Contract.Monad (liftedM)
import Contract.PlutusData (PlutusData(Integer))
import Contract.Test (ContractTest)
import Contract.Test.Assert
  ( checkExUnitsNotExceed
  , collectAssertionFailures
  , printContractAssertionFailures
  )
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip (InitialUTxOs, withWallets)
import Contract.Wallet
  ( ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  , withKeyWallet
  )
import Control.Monad.Trans.Class (lift)
import Ctl.Examples.ContractTestUtils as ContractTestUtils
import Ctl.Examples.Helpers (mkCurrencySymbol, mkTokenName)
import Ctl.Examples.PlutusV2.Scripts.AlwaysMints (alwaysMintsPolicyV2)
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Either (isLeft, isRight)
import Data.Newtype (wrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, test)
import Test.Ctl.Fixtures (cip25MetadataFixture1)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite = do
  group "Assertions interface" do
    let
      initialUtxos :: InitialUTxOs
      initialUtxos =
        [ BigInt.fromInt 2_000_000_000, BigInt.fromInt 2_000_000_000 ]

      distribution :: InitialUTxOs /\ InitialUTxOs
      distribution = initialUtxos /\ initialUtxos

    test "Successful run" do

      withWallets distribution \(alice /\ bob) -> do
        receiverPkh <- liftedM "Unable to get Bob's PKH" $
          head <$> withKeyWallet bob ownPaymentPubKeyHashes
        receiverSkh <- join <<< head <$> withKeyWallet bob ownStakePubKeyHashes

        mintingPolicy /\ cs <- mkCurrencySymbol alwaysMintsPolicyV2

        tn <- mkTokenName "TheToken"

        withKeyWallet alice do
          let
            params =
              { receiverPkh
              , receiverSkh
              , adaToSend: BigInt.fromInt 5_000_000
              , mintingPolicy
              , tokensToMint: cs /\ tn /\ one /\ unit
              , datumToAttach: wrap $ Integer $ BigInt.fromInt 42
              , txMetadata: cip25MetadataFixture1
              }

          checks <- ContractTestUtils.mkChecks params
          eiResult /\ failures <- collectAssertionFailures checks $ lift do
            ContractTestUtils.mkContract params
          eiResult `shouldSatisfy` isRight
          failures `shouldEqual` []

    test "Incorrect token value" do

      withWallets distribution \(alice /\ bob) -> do
        receiverPkh <- liftedM "Unable to get Bob's PKH" $
          head <$> withKeyWallet bob ownPaymentPubKeyHashes
        receiverSkh <- join <<< head <$> withKeyWallet bob ownStakePubKeyHashes

        mintingPolicy /\ cs <- mkCurrencySymbol alwaysMintsPolicyV2

        tn <- mkTokenName "TheToken"

        withKeyWallet alice do
          let
            params =
              { receiverPkh
              , receiverSkh
              , adaToSend: BigInt.fromInt 5_000_000
              , mintingPolicy
              , tokensToMint: cs /\ tn /\ one /\ unit
              , datumToAttach: wrap $ Integer $ BigInt.fromInt 42
              , txMetadata: cip25MetadataFixture1
              }

          checks <- ContractTestUtils.mkChecks params
            { tokensToMint = cs /\ tn /\ (one + one) /\ unit }
          eiResult /\ failures <- collectAssertionFailures checks $ lift do
            ContractTestUtils.mkContract params
          eiResult `shouldSatisfy` isRight
          printContractAssertionFailures failures `shouldEqual`
            "In addition to the error above, the following `Contract` assertions have failed:\n\n    1. Unexpected token delta (TokenName (hexToRawBytesUnsafe \"546865546f6b656e\")) at address Sender Expected: fromString \"2\", Actual: fromString \"1\""

    test "ExUnits limit reached" do

      withWallets distribution \(alice /\ bob) -> do
        receiverPkh <- liftedM "Unable to get Bob's PKH" $
          head <$> withKeyWallet bob ownPaymentPubKeyHashes
        receiverSkh <- join <<< head <$> withKeyWallet bob ownStakePubKeyHashes

        mintingPolicy /\ cs <- mkCurrencySymbol alwaysMintsPolicyV2

        tn <- mkTokenName "TheToken"

        withKeyWallet alice do
          let
            params =
              { receiverPkh
              , receiverSkh
              , adaToSend: BigInt.fromInt 5_000_000
              , mintingPolicy
              , tokensToMint: cs /\ tn /\ one /\ unit
              , datumToAttach: wrap $ Integer $ BigInt.fromInt 42
              , txMetadata: cip25MetadataFixture1
              }

          checks <- ContractTestUtils.mkChecks params <#>
            ( _ <>
                [ checkExUnitsNotExceed
                    { mem: BigInt.fromInt 800, steps: BigInt.fromInt 16110 }
                ]
            )
          eiResult /\ failures <- collectAssertionFailures checks $ lift do
            ContractTestUtils.mkContract params
          eiResult `shouldSatisfy` isRight
          printContractAssertionFailures failures `shouldEqual`
            "In addition to the error above, the following `Contract` assertions have failed:\n\n    1. ExUnits limit exceeded:  Expected: { mem: fromString \"800\", steps: fromString \"16110\" }, Actual: { mem: fromString \"800\", steps: fromString \"161100\" }"

    test "An exception is thrown - everything is reported" do

      withWallets distribution \(alice /\ bob) -> do
        receiverPkh <- liftedM "Unable to get Bob's PKH" $
          head <$> withKeyWallet bob ownPaymentPubKeyHashes
        receiverSkh <- join <<< head <$> withKeyWallet bob ownStakePubKeyHashes

        mintingPolicy /\ cs <- mkCurrencySymbol alwaysMintsPolicyV2

        tn <- mkTokenName "TheToken"

        withKeyWallet alice do
          let
            params =
              { receiverPkh
              , receiverSkh
              , adaToSend: BigInt.fromInt 5_000_000
              , mintingPolicy
              , tokensToMint: cs /\ tn /\ one /\ unit
              , datumToAttach: wrap $ Integer $ BigInt.fromInt 42
              , txMetadata: cip25MetadataFixture1
              }

          checks <-
            ContractTestUtils.mkChecks params
              { tokensToMint = cs /\ tn /\ (one + one) /\ unit } <#>
              ( _ <>
                  [ checkExUnitsNotExceed
                      { mem: BigInt.fromInt 800, steps: BigInt.fromInt 16110 }
                  ]
              )

          eiResult /\ failures <- collectAssertionFailures checks $ lift do
            ContractTestUtils.mkContract params <* liftEffect (throw ":(")

          eiResult `shouldSatisfy` isLeft
          printContractAssertionFailures failures `shouldEqual`
            "In addition to the error above, the following `Contract` assertions have failed:\n\n    1. Error while trying to get expected value: Unable to estimate expected loss in wallet\n\n    2. Unexpected token delta (TokenName (hexToRawBytesUnsafe \"546865546f6b656e\")) at address Sender Expected: fromString \"2\", Actual: fromString \"1\" \n\n    3. ExUnits limit exceeded:  Expected: { mem: fromString \"800\", steps: fromString \"16110\" }, Actual: { mem: fromString \"800\", steps: fromString \"161100\" } \n\nThe following `Contract` checks have been skipped due to an exception: \n\n    1. Sender's output has a datum\n\n    2. Output has a reference script\n\n    3. Contains CIP-25 metadata"
