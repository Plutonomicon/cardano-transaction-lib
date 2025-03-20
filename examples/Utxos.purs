module Ctl.Examples.Utxos (main, example, contract) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(Pay, MintAsset)
  )
import Cardano.Types
  ( Credential(PubKeyHashCredential)
  , OutputDatum(OutputDatum, OutputDatumHash)
  , PaymentCredential(PaymentCredential)
  , StakeCredential(StakeCredential)
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.Int as Int
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Contract.Address (mkAddress)
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo, logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractM
  , liftedM
  , runContract
  )
import Contract.PlutusData (PlutusData(Integer))
import Contract.Transaction
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  , awaitTxConfirmed
  , submitTxFromBuildPlan
  )
import Contract.Value (Value)
import Contract.Value (lovelaceValueOf, singleton) as Value
import Contract.Wallet
  ( getWalletUtxos
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  )
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Ctl.Examples.PlutusV2.OneShotMinting (oneShotMintingPolicyScriptV2)
import Data.Array (head) as Array
import Data.Log.Tag (tag)
import Data.Map (empty, toUnfoldable) as Map
import JS.BigInt (fromInt) as BigInt
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.Utxos"
  pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  skh <- ownStakePubKeyHash
  address <- mkAddress
    (PaymentCredential $ PubKeyHashCredential $ unwrap pkh)
    (StakeCredential <<< PubKeyHashCredential <<< unwrap <$> skh)

  datum <- liftEffect
    $ Integer
    <<< BigInt.fromInt
    <$> randomSampleOne arbitrary

  utxos <- liftedM "Failed to get UTxOs from wallet" getWalletUtxos
  oref <-
    liftContractM "Utxo set is empty"
      (map fst <<< Array.head <<< Map.toUnfoldable $ utxos)

  oneShotMintingPolicy <- oneShotMintingPolicyScriptV2 oref

  let cs0 = PlutusScript.hash oneShotMintingPolicy
  tn0 <- Helpers.mkAssetName "CTLNFT"

  let plutusScriptRef = PlutusScriptRef oneShotMintingPolicy
  nativeScriptRef <- liftEffect $ NativeScriptRef <$> randomSampleOne arbitrary

  let
    adaValue :: Value
    adaValue = Value.lovelaceValueOf (BigNum.fromInt 2_000_000)

    tokenValue = Value.singleton cs0 tn0 BigNum.one

    plan =
      [ MintAsset
          cs0
          tn0
          (Int.fromInt one)
          ( PlutusScriptCredential (ScriptValue oneShotMintingPolicy)
              RedeemerDatum.unit
          )
      , Pay $ TransactionOutput
          { address
          , amount: unsafePartial $ tokenValue <> adaValue
          , datum: Just $ OutputDatumHash $ hashPlutusData datum
          , scriptRef: Just plutusScriptRef
          }
      , Pay $ TransactionOutput
          { address
          , amount: adaValue
          , datum: Just $ OutputDatum datum
          , scriptRef: Just nativeScriptRef
          }
      ]

  tx <- submitTxFromBuildPlan Map.empty mempty plan
  awaitTxConfirmed $ Transaction.hash tx
  logInfo' "Tx submitted successfully!"

  utxos' <- liftedM "Failed to get UTxOs from wallet" getWalletUtxos
  logInfo (tag "utxos" $ show utxos') "Utxos after transaction confirmation:"
