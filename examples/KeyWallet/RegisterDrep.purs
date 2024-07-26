module Ctl.Examples.KeyWallet.RegisterDrep
  ( main
  ) where

import Contract.Prelude

import Ctl.Examples.Gov.RegisterDrep (contract) as Gov.RegisterDrep
import Ctl.Examples.KeyWallet.Internal.Contract (runKeyWalletContract)

main :: Effect Unit
main =
  runKeyWalletContract \unlock ->
    Gov.RegisterDrep.contract *> liftEffect unlock
