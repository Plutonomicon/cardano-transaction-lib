module Test.Ctl.Testnet.Gov
  ( suite
  ) where

import Prelude

import Cardano.Types.BigNum (fromInt) as BigNum
import Contract.Test (ContractTest)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Testnet (InitialUTxOs, withKeyWallet, withWallets)
import Ctl.Examples.Gov.ManageDrep (contract) as Gov.RegisterDrep
import Mote (group, skip, test)

suite :: TestPlanM ContractTest Unit
suite = do
  -- FIXME: It's not yet possible to start cardano-testnet in Conway era
  -- using its CLI
  skip $ group "Governance" do
    test "Registers as DRep (Gov.RegisterDrep example)" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigNum.fromInt 5_000_000
          , BigNum.fromInt 50_000_000
          ]
      withWallets distribution \alice ->
        withKeyWallet alice Gov.RegisterDrep.contract
