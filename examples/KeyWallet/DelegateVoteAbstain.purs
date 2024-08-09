module Ctl.Examples.KeyWallet.DelegateVoteAbstain
  ( main
  ) where

import Contract.Prelude

import Ctl.Examples.Gov.DelegateVoteAbstain (contract) as Gov.DelegateVoteAbstain
import Ctl.Examples.KeyWallet.Internal.Contract (runKeyWalletContract)

main :: Effect Unit
main =
  runKeyWalletContract \unlock ->
    Gov.DelegateVoteAbstain.contract *> liftEffect unlock
