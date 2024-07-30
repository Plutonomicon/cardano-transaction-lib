-- | Testing assertions interface provided by `Contract.Test.Assert`
module Test.Ctl.Testnet.Contract.Assert (suite) where

import Prelude

import Cardano.Types (ExUnits(ExUnits))
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Monad (liftedM)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (PlutusData(Integer))
import Contract.Test (ContractTest, InitialUTxOs, withWallets)
import Contract.Test.Assert
  ( checkExUnitsNotExceed
  , collectAssertionFailures
  , printContractAssertionFailures
  )
import Contract.Test.Mote (TestPlanM)
import Contract.Wallet
  ( ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  , withKeyWallet
  )
import Control.Monad.Trans.Class (lift)
import Ctl.Examples.ContractTestUtils as ContractTestUtils
import Ctl.Examples.Helpers (mkAssetName)
import Ctl.Examples.PlutusV2.Scripts.AlwaysMints (alwaysMintsPolicyScriptV2)
import Data.Array (head)
import Data.Either (isLeft, isRight)
import Data.Newtype (wrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import JS.BigInt as BigInt
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite = do
  group "Assertions interface" do
    let
      initialUtxos :: InitialUTxOs
      initialUtxos =
        [ BigNum.fromInt 2_000_000_000, BigNum.fromInt 2_000_000_000 ]

      distribution :: InitialUTxOs /\ InitialUTxOs
      distribution = initialUtxos /\ initialUtxos

    test "Successful run" do

      withWallets distribution \(alice /\ bob) -> do
        receiverPkh <- liftedM "Unable to get Bob's PKH" $
          head <$> withKeyWallet bob ownPaymentPubKeyHashes
        receiverSkh <- join <<< head <$> withKeyWallet bob ownStakePubKeyHashes

        mintingPolicy <- alwaysMintsPolicyScriptV2

        let cs = PlutusScript.hash mintingPolicy

        tn <- mkAssetName "TheToken"

        withKeyWallet alice do
          let
            params =
              { receiverPkh
              , receiverSkh
              , adaToSend: wrap $ BigNum.fromInt 5_000_000
              , mintingPolicy
              , tokensToMint: cs /\ tn /\ BigNum.one /\ unit
              , datumToAttach: Integer $ BigInt.fromInt 42
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

        mintingPolicy <- alwaysMintsPolicyScriptV2

        let cs = PlutusScript.hash mintingPolicy

        tn <- mkAssetName "TheToken"

        withKeyWallet alice do
          let
            params =
              { receiverPkh
              , receiverSkh
              , adaToSend: wrap $ BigNum.fromInt 5_000_000
              , mintingPolicy
              , tokensToMint: cs /\ tn /\ BigNum.one /\ unit
              , datumToAttach: Integer $ BigInt.fromInt 42
              }

          checks <- ContractTestUtils.mkChecks params
            { tokensToMint = cs /\ tn
                /\ (unsafePartial $ BigNum.one <> BigNum.one)
                /\ unit
            }
          eiResult /\ failures <- collectAssertionFailures checks $ lift do
            ContractTestUtils.mkContract params
          eiResult `shouldSatisfy` isRight
          printContractAssertionFailures failures `shouldEqual`
            "In addition to the error above, the following `Contract` assertions have failed:\n\n    1. Unexpected token delta (mkAssetName (hexToByteArrayUnsafe \"546865546f6b656e\")) at address Sender Expected: 2, Actual: 1"

    test "ExUnits limit reached" do

      withWallets distribution \(alice /\ bob) -> do
        receiverPkh <- liftedM "Unable to get Bob's PKH" $
          head <$> withKeyWallet bob ownPaymentPubKeyHashes
        receiverSkh <- join <<< head <$> withKeyWallet bob ownStakePubKeyHashes

        mintingPolicy <- alwaysMintsPolicyScriptV2

        let cs = PlutusScript.hash mintingPolicy

        tn <- mkAssetName "TheToken"

        withKeyWallet alice do
          let
            params =
              { receiverPkh
              , receiverSkh
              , adaToSend: wrap $ BigNum.fromInt 5_000_000
              , mintingPolicy
              , tokensToMint: cs /\ tn /\ BigNum.one /\ unit
              , datumToAttach: Integer $ BigInt.fromInt 42
              }

          checks <- ContractTestUtils.mkChecks params <#>
            ( _ <>
                [ checkExUnitsNotExceed
                    $ ExUnits
                        { mem: BigNum.fromInt 800, steps: BigNum.fromInt 16110 }
                ]
            )
          eiResult /\ failures <- collectAssertionFailures checks $ lift do
            ContractTestUtils.mkContract params
          eiResult `shouldSatisfy` isRight
          printContractAssertionFailures failures `shouldEqual`
            "In addition to the error above, the following `Contract` assertions have failed:\n\n    1. ExUnits limit exceeded:  Expected: { mem: 800, steps: 16110 }, Actual: { mem: 800, steps: 161100 }"

    test "An exception is thrown - everything is reported" do

      withWallets distribution \(alice /\ bob) -> do
        receiverPkh <- liftedM "Unable to get Bob's PKH" $
          head <$> withKeyWallet bob ownPaymentPubKeyHashes
        receiverSkh <- join <<< head <$> withKeyWallet bob ownStakePubKeyHashes

        mintingPolicy <- alwaysMintsPolicyScriptV2

        let cs = PlutusScript.hash mintingPolicy

        tn <- mkAssetName "TheToken"

        withKeyWallet alice do
          let
            params =
              { receiverPkh
              , receiverSkh
              , adaToSend: wrap $ BigNum.fromInt 5_000_000
              , mintingPolicy
              , tokensToMint: cs /\ tn /\ BigNum.one /\ unit
              , datumToAttach: Integer $ BigInt.fromInt 42
              }

          checks <-
            ContractTestUtils.mkChecks params
              { tokensToMint = cs /\ tn
                  /\ (unsafePartial $ BigNum.one <> BigNum.one)
                  /\ unit
              } <#>
              ( _ <>
                  [ checkExUnitsNotExceed $ ExUnits
                      { mem: BigNum.fromInt 800, steps: BigNum.fromInt 16110 }
                  ]
              )

          eiResult /\ failures <- collectAssertionFailures checks $ lift do
            ContractTestUtils.mkContract params <* liftEffect (throw ":(")

          eiResult `shouldSatisfy` isLeft
          printContractAssertionFailures failures `shouldEqual`
            "In addition to the error above, the following `Contract` assertions have failed:\n\n    1. Error while trying to get expected value: Unable to estimate expected loss in wallet\n\n    2. Unexpected token delta (mkAssetName (hexToByteArrayUnsafe \"546865546f6b656e\")) at address Sender Expected: 2, Actual: 1 \n\n    3. ExUnits limit exceeded:  Expected: { mem: 800, steps: 16110 }, Actual: { mem: 800, steps: 161100 } \n\nThe following `Contract` checks have been skipped due to an exception: \n\n    1. Sender's output has a datum\n\n    2. Output has a reference script"
