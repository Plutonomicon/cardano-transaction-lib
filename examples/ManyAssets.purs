-- | Mints many assets at once. A reproduction for https://github.com/Plutonomicon/cardano-transaction-lib/issues/1441
module Ctl.Examples.ManyAssets
  ( contract
  , example
  , main
  , mkContractWithAssertions
  ) where

import Contract.Prelude

import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Config
  ( ContractParams
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  )
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractM
  , liftedM
  , runContract
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Wallet (getWalletUtxos)
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Ctl.Examples.PlutusV2.Scripts.AlwaysMints (alwaysMintsPolicyScriptV2)
import Data.Array (head, range) as Array
import Data.Map (toUnfoldable) as Map

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec = Just $ ConnectToGenericCip30 "nami" { cip95: false }
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
  utxos <- liftedM "Failed to get UTxOs from wallet" getWalletUtxos
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (Map.toUnfoldable utxos :: Array _))

  mp <- alwaysMintsPolicyScriptV2
  let cs = PlutusScript.hash mp
  tns <- for (Array.range 0 600) \i -> Helpers.mkAssetName $ "CTLNFT" <> show i

  let
    constraints :: Constraints.TxConstraints
    constraints =
      fold
        ( tns <#> \tn -> Constraints.mustMintValue
            (Mint.singleton cs tn $ Int.fromInt one)
        )
        <> Constraints.mustSpendPubKeyOutput oref

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.plutusMintingPolicy mp
      <> Lookups.unspentOutputs utxos

  txHash <- submitTxFromConstraints lookups constraints
  logInfo' $ "Tx ID: " <> show txHash
  awaitTxConfirmed txHash
  logInfo' "Tx submitted successfully!"
