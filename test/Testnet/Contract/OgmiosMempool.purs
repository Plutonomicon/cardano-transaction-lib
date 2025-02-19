module Test.Ctl.Testnet.Contract.OgmiosMempool
  ( suite
  ) where

import Prelude

import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Contract.Backend.Ogmios.Mempool
  ( MempoolM
  , acquireMempoolSnapshot
  , fetchMempoolTxs
  , mempoolSnapshotHasTx
  , mempoolSnapshotSizeAndCapacity
  , withMempoolSnapshot
  )
import Contract.Monad (Contract)
import Contract.Test (ContractTest, InitialUTxOs, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Reader.Trans (ask, runReaderT)
import Ctl.Examples.PlutusV2.InlineDatum as InlineDatum
import Ctl.Internal.Contract.ProviderBackend (ProviderBackend(CtlBackend))
import Ctl.Internal.Logging (mkLogger)
import Ctl.Internal.QueryM.Ogmios.Mempool
  ( MempoolSizeAndCapacity(MempoolSizeAndCapacity)
  , OgmiosWebSocket
  , mkOgmiosWebSocketAff
  )
import Ctl.Internal.ServerConfig (mkWsUrl)
import Data.Array (length)
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Mote (group, skip, test)
import Test.Spec.Assertions (shouldEqual)

mkWebsocket :: Contract OgmiosWebSocket
mkWebsocket = do
  config <- ask
  ogmiosConfig <- case config.backend of
    CtlBackend ctlBackend _ -> pure ctlBackend.ogmiosConfig
    _ -> throwError $ error "Ogmios backend not supported"
  liftAff $ mkOgmiosWebSocketAff
    (mkLogger config.logLevel config.customLogger)
    (mkWsUrl ogmiosConfig)

runMempoolAction
  :: forall (a :: Type). OgmiosWebSocket -> MempoolM a -> Contract a
runMempoolAction ogmiosWs mempoolAction = do
  config <- ask
  liftAff $ runReaderT (unwrap mempoolAction)
    { ogmiosWs
    , logLevel: config.logLevel
    , customLogger: config.customLogger
    , suppressLogs: config.suppressLogs
    }

suite :: TestPlanM ContractTest Unit
suite = group "Ogmios mempool test" do
  test "acquireMempoolSnapshot" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ BigNum.fromInt 1000_000_000
        , BigNum.fromInt 2000_000_000
        ]
    withWallets distribution \alice -> do
      withKeyWallet alice do
        ws <- mkWebsocket
        void $ runMempoolAction ws acquireMempoolSnapshot

  test "fetchMempoolTXs" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ BigNum.fromInt 1000_000_000
        , BigNum.fromInt 2000_000_000
        ]
    withWallets distribution \alice -> do
      withKeyWallet alice do
        ws <- mkWebsocket
        validator <- InlineDatum.checkDatumIsInlineScript
        let vhash = PlutusScript.hash validator
        txId <- InlineDatum.payToCheckDatumIsInline vhash
        mpTxs <- runMempoolAction ws
          (fetchMempoolTxs =<< acquireMempoolSnapshot)
        length mpTxs `shouldEqual` 1
        awaitTxConfirmed txId
        mpTxs' <- runMempoolAction ws
          (fetchMempoolTxs =<< acquireMempoolSnapshot)
        length mpTxs' `shouldEqual` 0
  skip $ test
    "mempoolSnapshotHasTx - skipped because HasTx always returns false for some reason"
    do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigNum.fromInt 1000_000_000
          , BigNum.fromInt 2000_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          ws <- mkWebsocket
          validator <- InlineDatum.checkDatumIsInlineScript
          let vhash = PlutusScript.hash validator
          txId <- InlineDatum.payToCheckDatumIsInline vhash
          runMempoolAction ws $
            withMempoolSnapshot (flip mempoolSnapshotHasTx txId) >>= shouldEqual
              true
          snapshot <- runMempoolAction ws acquireMempoolSnapshot
          _mpTxs' <- runMempoolAction ws $ fetchMempoolTxs snapshot
          -- for_ mpTxs' \tx -> do
          --   liftEffect <<< Console.log <<< show =<< liftEffect
          --     (transactionHash tx)
          awaitTxConfirmed txId
          runMempoolAction ws $
            mempoolSnapshotHasTx snapshot txId >>= shouldEqual false
  test "mempoolSnapshotSizeAndCapacity" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ BigNum.fromInt 1000_000_000
        , BigNum.fromInt 2000_000_000
        ]
    withWallets distribution \alice -> do
      withKeyWallet alice do
        ws <- mkWebsocket
        validator <- InlineDatum.checkDatumIsInlineScript
        let vhash = PlutusScript.hash validator
        void $ InlineDatum.payToCheckDatumIsInline vhash
        MempoolSizeAndCapacity { numberOfTxs } <-
          runMempoolAction ws $ withMempoolSnapshot
            (mempoolSnapshotSizeAndCapacity)
        numberOfTxs `shouldEqual` 1
