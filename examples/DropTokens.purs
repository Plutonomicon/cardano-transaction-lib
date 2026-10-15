-- | This module can be used to get rid of extra tokens. It drops half of the
-- | tokens by sending them to a null wallet.
module Ctl.Examples.DropTokens (main, example, contract) where

import Contract.Prelude

import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.Value as Value
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.TxConstraints as Helpers
import Contract.Value (singleton)
import Contract.Wallet (getWalletBalance)
import Data.Array as Array
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Fixtures (nullPaymentPubKeyHash)

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
  logInfo' "Running Examples.DropTokens"
  logInfo' "Going to get rid of half of the tokens available"
  value <- unsafePartial $ getWalletBalance <#> fold

  let
    halfArray arr = Array.take (Array.length arr `div` 2) arr
    -- drop half of the tokens
    tokenValueHalf =
      unsafePartial $ fold
        $ halfArray (MultiAsset.flatten $ Value.getMultiAsset value)
        <#>
          \(cs /\ tn /\ n) -> singleton cs tn n

    constraints :: Constraints.TxConstraints
    constraints = Helpers.mustPayToPubKey nullPaymentPubKeyHash tokenValueHalf

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  submitTxFromConstraints lookups constraints >>= awaitTxConfirmed
  logInfo' "Tx submitted successfully!"
