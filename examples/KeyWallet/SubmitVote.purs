module Ctl.Examples.KeyWallet.SubmitVote
  ( main
  ) where

import Contract.Prelude

import Ctl.Examples.Gov.SubmitVote (contract) as Gov.SubmitVote
import Ctl.Examples.KeyWallet.Internal.Contract (runKeyWalletContract)

main :: Effect Unit
main =
  runKeyWalletContract \unlock ->
    Gov.SubmitVote.contract *> liftEffect unlock
