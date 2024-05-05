-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a token using the `AlwaysMints` policy and sends it along with
-- | the selected amount to the specified address.
module Ctl.Examples.KeyWallet.MintsAndSendsToken (main) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Log (logInfo')
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Examples.AlwaysMints (alwaysMintsPolicy)
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Ctl.Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = runKeyWalletContract_ \pkh lovelace unlock -> do
  logInfo' "Running Examples.KeyWallet.MintsAndSendsToken"

  mp <- alwaysMintsPolicy
  let cs = PlutusScript.hash mp
  tn <- Helpers.mkAssetName "TheToken"

  let
    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue (Mint.singleton cs tn Int.one)
      , Constraints.mustPayToPubKey pkh
          ( unsafePartial $ Value.lovelaceValueOf lovelace <> Value.singleton cs
              tn
              BigNum.one
          )
      ]

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.plutusMintingPolicy mp

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  liftEffect unlock
