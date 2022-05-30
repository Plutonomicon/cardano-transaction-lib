-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a transaction. It creates a simple transaction that gets
-- | UTxOs from the user's wallet and sends two Ada back to the same wallet address
module Examples.Pkh2PkhKeyWallet (main) where

import Contract.Prelude

import Contract.Address (NetworkId(TestnetId))
import Contract.Monad
  ( ConfigParams(ConfigParams)
  , Contract
  , ContractConfig
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , defaultSlotConfig
  , launchAff_
  , liftedE
  , liftedM
  , logInfo'
  , mkContractConfig
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Control.Monad.Error.Class (catchError, liftMaybe)
import Control.Monad.Logger.Trans (runLoggerT)
import Control.Monad.Reader (runReaderT)
import Data.BigInt as BigInt
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Level (LogLevel(..))
import Data.Log.Message (Message)
import Effect.Exception (error, message)
import QueryM (QueryConfig)
import Serialization (privateKeyFromBytes)
import Serialization.Address (addressFromBech32)
import Serialization.Hash (ed25519KeyHashFromBech32)
import Types.ByteArray (hexToByteArray)
import Wallet (mkKeyWallet)

type Form =
  { private_key :: String
  , public_key :: String
  , to_pkh :: String
  }

foreign import mkForm
  :: (Form -> (String -> String -> Effect Unit) -> (Effect Unit) -> Effect Unit)
  -> Effect Unit

levelName :: LogLevel -> String
levelName Trace = "TRACE"
levelName Debug = "DEBUG"
levelName Info = "INFO"
levelName Warn = "WARN"
levelName Error = "ERROR"

levelColor :: LogLevel -> String
levelColor Warn = "gold"
levelColor Error = "crimson"
levelColor _ = "black"

main :: Effect Unit
main = do
  mkForm \input log' unlock -> do
    let
      runContract
        :: forall (r :: Row Type) (a :: Type)
         . ContractConfig r
        -> Contract r a
        -> Aff a
      runContract config = flip runLoggerT printLog <<< flip runReaderT cfg <<<
        unwrap
        where
        printLog :: Message -> Aff Unit
        printLog m = liftEffect $ when (m.level >= cfg.logLevel) $ do
          prettyFormatter m >>= log
          log' (levelColor m.level)
            ("[" <> levelName m.level <> "] " <> m.message)

        cfg :: QueryConfig r
        cfg = unwrap config

      -- | Same as `runContract` discarding output.
      runContract_
        :: forall (r :: Row Type) (a :: Type)
         . ContractConfig r
        -> Contract r a
        -> Aff Unit
      runContract_ config = void <<< runContract config

    launchAff_ $ flip catchError
      (\e -> liftEffect $ log' "crimson" ("[ERROR] " <> message e) *> unlock)
      do
        priv <- (liftMaybe (error "Failed to parse Private Key")) =<<
          ( liftEffect $ map join $ traverse privateKeyFromBytes $
              hexToByteArray input.private_key
          )
        pub <- liftMaybe (error "Failed to parse Public Key") $
          addressFromBech32 input.public_key
        pkh <- liftMaybe (error "Failed to parse Public key Hash") $
          ed25519KeyHashFromBech32
            "addr_vkh14lk6mlsm50ewtn9p5zgd7lkfdalsycdcxjpl5s979st9xjdmdc4"
        let wallet = mkKeyWallet pub priv
        cfg <- mkContractConfig $ ConfigParams
          { ogmiosConfig: defaultOgmiosWsConfig
          , datumCacheConfig: defaultDatumCacheWsConfig
          , ctlServerConfig: defaultServerConfig
          , networkId: TestnetId
          , slotConfig: defaultSlotConfig
          , logLevel: Trace
          , extraConfig: {}
          , wallet: Just wallet
          }

        runContract_ cfg do
          logInfo' "Running Examples.Pkh2PkhKeyWallet"

          let
            constraints :: Constraints.TxConstraints Void Void
            constraints = Constraints.mustPayToPubKey (wrap (wrap pkh))
              $ Value.lovelaceValueOf
              $ BigInt.fromInt 2_000_000

            lookups :: Lookups.ScriptLookups Void
            lookups = mempty

          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          BalancedSignedTransaction bsTx <-
            liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
          txId <- submit bsTx.signedTxCbor
          logInfo' $ "Tx ID: " <> show txId
          liftEffect unlock
