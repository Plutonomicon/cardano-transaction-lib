-- | Mints many assets at once. A reproduction for https://github.com/Plutonomicon/cardano-transaction-lib/issues/1441
module Ctl.Examples.ManyAssets
  ( contract
  , example
  , main
  , mkContractWithAssertions
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(MintAsset)
  )
import Cardano.Types.Int as Int
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Transaction (awaitTxConfirmed, submitTxFromBuildPlan)
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Ctl.Examples.PlutusV2.Scripts.AlwaysMints (alwaysMintsPolicyScriptV2)
import Data.Array (range) as Array
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
contract =
  mkContractWithAssertions "Examples.ManyAssets"

mkContractWithAssertions
  :: String
  -> Contract Unit
mkContractWithAssertions exampleName = do
  logInfo' ("Running " <> exampleName)
  mp <- alwaysMintsPolicyScriptV2
  let cs = PlutusScript.hash mp
  tns <- for (Array.range 0 600) \i -> Helpers.mkAssetName $ "CTLNFT" <> show i

  let
    plan =
      tns <#> \tn -> MintAsset cs tn (Int.fromInt one)
        (PlutusScriptCredential (ScriptValue mp) RedeemerDatum.unit)

  txHash <- Transaction.hash <$> submitTxFromBuildPlan Map.empty mempty plan
  logInfo' $ "Tx ID: " <> show txHash
  awaitTxConfirmed txHash
  logInfo' "Tx submitted successfully!"
