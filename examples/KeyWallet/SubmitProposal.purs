module Ctl.Examples.KeyWallet.SubmitProposal
  ( main
  ) where

import Contract.Prelude

import Ctl.Examples.Gov.SubmitProposal (contract) as Gov.SubmitProposal
import Ctl.Examples.KeyWallet.Internal.Contract (runKeyWalletContract)

main :: Effect Unit
main =
  runKeyWalletContract \unlock ->
    Gov.SubmitProposal.contract *> liftEffect unlock
