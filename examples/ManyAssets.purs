-- | Mints many assets at once. A reproduction for https://github.com/Plutonomicon/cardano-transaction-lib/issues/1441
module Ctl.Examples.ManyAssets
  ( contract
  , example
  , main
  , mkContractWithAssertions
  ) where

import Contract.Prelude

import Contract.Config (ContractParams, testnetNamiConfig)
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
import Contract.Value (singleton) as Value
import Contract.Wallet (getWalletUtxos)
import Ctl.Examples.Helpers (mkCurrencySymbol, mkTokenName) as Helpers
import Ctl.Examples.PlutusV2.Scripts.AlwaysMints (alwaysMintsPolicyV2)
import Data.Array (head, range) as Array
import Data.Map (toUnfoldable) as Map

main :: Effect Unit
main = example testnetNamiConfig

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

  mp /\ cs <- Helpers.mkCurrencySymbol (alwaysMintsPolicyV2)
  tns <- for (Array.range 0 600) \i -> Helpers.mkTokenName $ "CTLNFT" <> show i

  let
    constraints :: Constraints.TxConstraints
    constraints =
      fold
        (tns <#> \tn -> Constraints.mustMintValue (Value.singleton cs tn one))
        <> Constraints.mustSpendPubKeyOutput oref

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.mintingPolicy mp
      <> Lookups.unspentOutputs utxos

  txHash <- submitTxFromConstraints lookups constraints
  logInfo' $ "Tx ID: " <> show txHash
  awaitTxConfirmed txHash
  logInfo' "Tx submitted successfully!"
