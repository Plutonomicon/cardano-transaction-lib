module Test.FinalizeTx where

import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Mote (group, test)
import QueryM
  ( FinalizedTransaction(FinalizedTransaction)
  , finalizeTx
  , runQueryM
  , traceQueryConfig
  )
import Test.Fixtures (plutusDataFixture6, redeemerFixture1, txFixture1)
import TestM (TestPlanM)
import Types.ByteArray (byteArrayToHex)
import Types.Datum (Datum(Datum))

suite :: TestPlanM Unit
suite = do
  group "finalizeTx" do
    test "Call finalize" do
      qcf <- liftAff $ traceQueryConfig
      res <- liftAff $ runQueryM qcf do
        finalizeTx txFixture1 [ Datum plutusDataFixture6 ] [ redeemerFixture1 ]
      log $ show $ res <#> \(FinalizedTransaction bytes) ->
        byteArrayToHex bytes
      pure unit
