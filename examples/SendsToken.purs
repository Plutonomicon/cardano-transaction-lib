-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit transactions. It creates two transactions: one that
-- | mints a token and one that sends that token to the owner's address.

module Ctl.Examples.SendsToken (main, example, contract) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(Pay, MintAsset)
  )
import Cardano.Types
  ( AssetName
  , Credential(PubKeyHashCredential)
  , PaymentCredential(PaymentCredential)
  , PlutusScript
  , ScriptHash
  , StakeCredential(StakeCredential)
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.BigNum as BigNum
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
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromBuildPlan
  )
import Contract.Value (Value)
import Contract.Value as Value
import Contract.Wallet (ownPaymentPubKeyHashes, ownStakePubKeyHashes)
import Ctl.Examples.AlwaysMints (alwaysMintsPolicy)
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Data.Array (head)
import Data.Map as Map

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.SendsToken"

  mintToken >>= awaitTxConfirmed
  logInfo' "Tx submitted successfully, Sending token to own address"

  sendToken >>= awaitTxConfirmed
  logInfo' "Tx submitted successfully!"

mintToken :: Contract TransactionHash
mintToken = do
  mp /\ sh /\ an /\ amount /\ _value <- tokenValue

  tx <- submitTxFromBuildPlan Map.empty mempty
    [ MintAsset
        sh
        an
        amount
        (PlutusScriptCredential (ScriptValue mp) RedeemerDatum.unit)
    ]
  pure $ Transaction.hash tx

sendToken :: Contract TransactionHash
sendToken = do
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  skh <- join <<< head <$> ownStakePubKeyHashes
  _ /\ _ /\ _ /\ _ /\ value <- tokenValue
  address <- mkAddress (PaymentCredential $ PubKeyHashCredential $ unwrap pkh)
    (StakeCredential <<< PubKeyHashCredential <<< unwrap <$> skh)
  tx <- submitTxFromBuildPlan Map.empty mempty
    [ Pay $ TransactionOutput
        { address
        , amount: value
        , datum: Nothing
        , scriptRef: Nothing
        }
    ]
  pure $ Transaction.hash tx

tokenValue
  :: Contract (PlutusScript /\ ScriptHash /\ AssetName /\ Int.Int /\ Value)
tokenValue = do
  mp <- alwaysMintsPolicy
  let cs = PlutusScript.hash mp
  an <- Helpers.mkAssetName "TheToken"
  pure $ mp /\ cs /\ an /\ Int.fromInt 1 /\ Value.singleton cs an
    (BigNum.fromInt 1)
