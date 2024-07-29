module Ctl.Examples.KeyWallet.SubmitVoteSimple
  ( main
  ) where

import Contract.Prelude

import Ctl.Examples.Gov.SubmitVoteSimple (contract) as Gov.SubmitVoteSimple
import Ctl.Examples.KeyWallet.Internal.Contract (runKeyWalletContract)

main :: Effect Unit
main =
  runKeyWalletContract \unlock ->
    Gov.SubmitVoteSimple.contract *> liftEffect unlock
