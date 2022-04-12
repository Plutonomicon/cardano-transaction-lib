module Test.FinalizeTx where

import Prelude

import Control.Monad.Reader.Trans (runReaderT)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Mote (group, test)
import QueryM
  ( FinalizedTransaction(FinalizedTransaction)
  , traceQueryConfig
  , finalizeTx
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
      res <- liftAff $ flip runReaderT qcf do
        finalizeTx txFixture1 [ Datum plutusDataFixture6 ] [ redeemerFixture1 ]
      liftEffect $ Console.log $ show $ res <#> \(FinalizedTransaction bytes) ->
        byteArrayToHex bytes
      pure unit
