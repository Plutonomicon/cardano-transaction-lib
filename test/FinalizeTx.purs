module Test.FinalizeTx where

import Prelude

import Address (addressToOgmiosAddress, ogmiosAddressToAddress)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, test)
import QueryM
  ( defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , mkDatumCacheWebSocketAff
  , mkOgmiosWebSocketAff
  )
import QueryM.Utxos (utxosAt)
import Serialization.Address (NetworkId(TestnetId))
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Types.Interval (defaultSlotConfig)
import Types.JsonWsp (OgmiosAddress)
import UsedTxOuts (newUsedTxOuts)
import Test.Fixtures

import Effect.Console as Console
import QueryM
import Effect.Aff.Class

suite :: TestPlanM Unit
suite = do
  -- Test UtxosAt using internal types.
  group "Aff Int" do
    test "Call finalize" do
      qcf <- liftAff $ getQueryConfig
      res <- liftAff $ flip runReaderT qcf do
        finalizeTx txFixture1
      liftEffect $ Console.log $ show res
      pure unit

getQueryConfig :: Aff QueryConfig
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
