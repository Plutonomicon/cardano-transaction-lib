module Test.FinalizeTx where

import Prelude

import Control.Monad.Reader.Trans (runReaderT)
import Data.Maybe (Maybe(Nothing))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Mote (group, test)
import QueryM
  ( DefaultQueryConfig
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , FinalizedTransaction(..)
  , finalizeTx
  , mkDatumCacheWebSocketAff
  , mkOgmiosWebSocketAff
  )
import Serialization.Address (NetworkId(TestnetId))
import Test.Fixtures (plutusDataFixture6, redeemerFixture1, txFixture1)
import TestM (TestPlanM)
import Types.ByteArray (byteArrayToHex)
import Types.Datum (Datum(Datum))
import Types.Interval (defaultSlotConfig)
import Types.UsedTxOuts (newUsedTxOuts)

suite :: TestPlanM Unit
suite = do
  group "finalizeTx" do
    test "Call finalize" do
      qcf <- liftAff $ getQueryConfig
      res <- liftAff $ flip runReaderT qcf do
        finalizeTx txFixture1 [ Datum plutusDataFixture6 ] [ redeemerFixture1 ]
      liftEffect $ Console.log $ show $ res <#> \(FinalizedTransaction bytes) ->
        byteArrayToHex bytes
      pure unit

getQueryConfig :: Aff DefaultQueryConfig
getQueryConfig = do
  ogmiosWs <- mkOgmiosWebSocketAff defaultOgmiosWsConfig
  datumCacheWs <- mkDatumCacheWebSocketAff defaultDatumCacheWsConfig
  usedTxOuts <- newUsedTxOuts
  pure
    { ogmiosWs
    , datumCacheWs
    , serverConfig: defaultServerConfig
    , wallet: Nothing
    , usedTxOuts
    , networkId: TestnetId
    , slotConfig: defaultSlotConfig
    }
