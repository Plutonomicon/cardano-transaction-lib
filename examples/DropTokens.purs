-- | This module can be used to get rid of extra tokens. It drops half of the
-- | tokens by sending them to a null wallet.
module Ctl.Examples.DropTokens (main, example, contract) where

import Contract.Prelude

import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.TxConstraints as Helpers
import Contract.Value
  ( coinToValue
  , flattenNonAdaAssets
  , negation
  , singleton
  , valueToCoin
  )
import Contract.Wallet (getWalletBalance)
import Data.Array as Array
import Test.Ctl.Fixtures (nullPaymentPubKeyHash)

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.DropTokens"
  logInfo' "Going to get rid of half of the tokens available"
  value <- getWalletBalance <#> fold

  let
    tokenValue = value <> negation (coinToValue (valueToCoin value))
    halfArray arr = Array.take (Array.length arr `div` 2) arr
    -- drop half of the tokens
    tokenValueHalf =
      fold $ halfArray (flattenNonAdaAssets tokenValue) <#>
        \(cs /\ tn /\ n) -> singleton cs tn n

    constraints :: Constraints.TxConstraints
    constraints = Helpers.mustPayToPubKey nullPaymentPubKeyHash tokenValueHalf

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  submitTxFromConstraints lookups constraints >>= awaitTxConfirmed
  logInfo' "Tx submitted successfully!"
