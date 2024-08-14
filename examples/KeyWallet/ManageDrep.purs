module Ctl.Examples.KeyWallet.ManageDrep
  ( main
  ) where

import Contract.Prelude

import Ctl.Examples.Gov.ManageDrep (contract) as Gov.ManageDrep
import Ctl.Examples.KeyWallet.Internal.Contract (runKeyWalletContract)

main :: Effect Unit
main =
  runKeyWalletContract \unlock ->
    Gov.ManageDrep.contract *> liftEffect unlock
