module Ctl.Examples.Gov.ManageDrepScript
  ( ContractPath(RegDrep, UpdateDrep, UnregDrep)
  , contract
  , contractStep
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(IssueCertificate)
  )
import Cardano.Types
  ( Anchor
  , Certificate(RegDrepCert, UpdateDrepCert, UnregDrepCert)
  , Credential(ScriptHashCredential)
  , ScriptHash
  )
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.RedeemerDatum (unit) as RedeemerDatum
import Cardano.Types.Transaction (hash) as Transaction
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction (awaitTxConfirmed, submitTxFromBuildPlan)
import Control.Monad.Error.Class (catchError, throwError)
import Ctl.Examples.Gov.Internal.Common (dummyAnchor)
import Ctl.Examples.PlutusV3.Scripts.AlwaysMints (alwaysMintsPolicyScriptV3)
import Data.Map (empty) as Map
import Data.String (Pattern(Pattern))
import Data.String (contains) as String
import Effect.Exception (message)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: true }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.Gov.ManageDrepScript"
  drepScriptHash <- contractStep RegDrep
  logInfo' $ "Successfully registered DRep. DRepID: " <> show drepScriptHash
  void $ contractStep $ UpdateDrep dummyAnchor
  logInfo' "Successfully updated DRep metadata."
  void $ contractStep UnregDrep
  logInfo' "Successfully unregistered DRep."

data ContractPath
  = RegDrep
  | UpdateDrep Anchor
  | UnregDrep

contractStep :: ContractPath -> Contract ScriptHash
contractStep path = do
  drepScript <- alwaysMintsPolicyScriptV3
  let
    drepScriptHash = PlutusScript.hash drepScript
    drepCred = ScriptHashCredential drepScriptHash
    drepCredWitness = PlutusScriptCredential (ScriptValue drepScript)
      RedeemerDatum.unit

  drepDeposit <- _.drepDeposit <<< unwrap <$> getProtocolParameters

  let
    submitTx = do
      tx <- submitTxFromBuildPlan Map.empty mempty
        [ case path of
            RegDrep ->
              IssueCertificate (RegDrepCert drepCred drepDeposit Nothing)
                (Just drepCredWitness)
            UpdateDrep anchor ->
              IssueCertificate (UpdateDrepCert drepCred $ Just anchor)
                (Just drepCredWitness)
            UnregDrep ->
              IssueCertificate (UnregDrepCert drepCred drepDeposit)
                (Just drepCredWitness)
        ]
      awaitTxConfirmed $ Transaction.hash tx

  submitTx `catchError` \err ->
    unless
      (String.contains (Pattern "knownDelegateRepresentative") $ message err)
      (throwError err)

  pure drepScriptHash
