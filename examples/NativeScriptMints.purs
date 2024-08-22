-- | Example contract that uses a single signer native script as a minting
-- | policy
module Ctl.Examples.NativeScriptMints (main, example, contract, pkhPolicy) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( CredentialWitness(NativeScriptCredential)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(Pay, MintAsset)
  )
import Cardano.Types
  ( BigNum
  , Credential(PubKeyHashCredential)
  , PaymentCredential(PaymentCredential)
  , StakeCredential(StakeCredential)
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.Transaction as Transaction
import Contract.Address (PaymentPubKeyHash, mkAddress)
import Contract.Config
  ( ContractParams
  , KnownWallet(Nami)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.Scripts (NativeScript(ScriptPubkey))
import Contract.Transaction (awaitTxConfirmed, submitTxFromBuildPlan)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Contract.Wallet (ownPaymentPubKeyHashes, ownStakePubKeyHashes)
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Data.Array (head)
import Data.Map as Map
import JS.BigInt as BigInt

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Nami) { cip95: false }
  }

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.NativeScriptMints"

  pkh <- liftedM "Couldn't get own pkh" $ head <$> ownPaymentPubKeyHashes

  let mintingPolicy = pkhPolicy pkh
  let scriptHash = NativeScript.hash mintingPolicy
  assetName <- Helpers.mkAssetName "NSToken"

  txId <- Transaction.hash <$> submitTxFromBuildPlan Map.empty mempty
    [ MintAsset
        scriptHash
        assetName
        (Int.fromInt 100)
        (NativeScriptCredential (ScriptValue mintingPolicy))
    ]

  awaitTxConfirmed txId
  logInfo' "Minted successfully"

  toSelfContract scriptHash assetName $ BigNum.fromInt 50

toSelfContract :: CurrencySymbol -> TokenName -> BigNum -> Contract Unit
toSelfContract cs tn amount = do
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  skh <- join <<< head <$> ownStakePubKeyHashes
  address <- mkAddress
    (PaymentCredential $ PubKeyHashCredential $ unwrap pkh)
    (StakeCredential <<< PubKeyHashCredential <<< unwrap <$> skh)
  let
    plan =
      [ Pay $ TransactionOutput
          { address
          , amount: Value.singleton cs tn amount
          , datum: Nothing
          , scriptRef: Nothing
          }
      ]

  tx <- submitTxFromBuildPlan Map.empty mempty plan

  awaitTxConfirmed $ Transaction.hash tx
  logInfo' $ "Moved " <> show (BigInt.fromInt 50) <> " to self successfully"

example :: ContractParams -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract

pkhPolicy :: PaymentPubKeyHash -> NativeScript
pkhPolicy = ScriptPubkey <<< unwrap
