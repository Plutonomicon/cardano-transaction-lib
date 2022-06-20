-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a transaction. It creates a simple transaction that gets
-- | UTxOs from the user's wallet and sends the selected amount to the specified
-- | address
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
  , launchAff_
  , liftedE
  , liftedM
  , logInfo'
  , mkContractConfig
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction (balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Control.Monad.Error.Class (catchError, liftMaybe)
import Control.Monad.Logger.Trans (runLoggerT)
import Control.Monad.Reader (runReaderT)
import Data.BigInt as BigInt
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Level (LogLevel(Trace, Debug, Warn, Info, Error))
import Data.Log.Message (Message)
import Effect.Exception (Error, error, message)
import QueryM (QueryConfig)
import Serialization (privateKeyFromBytes)
import Serialization.Hash (ed25519KeyHashFromBech32)
import Types.RawBytes (hexToRawBytes)
import Wallet (mkKeyWallet)

type Form =
  { privateKey :: String
  , toPkh :: String
  , lovelace :: String
  }

type Log = String -> String -> Effect Unit

type Unlock = Effect Unit

foreign import mkForm
  :: (Form -> Log -> Unlock -> Effect Unit)
  -> Effect Unit

foreign import logError
  :: Error -> Effect Unit

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
      runContract_
        :: forall r
         . ContractConfig r
        -> Contract r Unit
        -> Aff Unit
      runContract_ config = flip runLoggerT printLog <<< flip runReaderT cfg <<<
        unwrap
        where
        printLog :: Message -> Aff Unit
        printLog m = liftEffect $ when (m.level >= cfg.logLevel) $ do
          prettyFormatter m >>= log
          log' (levelColor m.level)
            ("[" <> levelName m.level <> "] " <> m.message)

        cfg :: QueryConfig r
        cfg = unwrap config

    launchAff_ $ flip catchError
      ( \e -> liftEffect $ logError e
          *> log' "crimson" ("[ERROR] " <> message e)
          *> unlock
      )
      do
        priv <- (liftMaybe $ error "Failed to parse private key") =<<
          ( liftEffect $ map join $ traverse privateKeyFromBytes $
              hexToRawBytes input.privateKey
          )
        pkh <- liftMaybe (error "Failed to parse public key hash") $
          ed25519KeyHashFromBech32 input.toPkh
        lovelace <- liftMaybe (error "Failed to parse lovelace amount") $
          BigInt.fromString input.lovelace
        let wallet = mkKeyWallet priv
        cfg <- mkContractConfig $ ConfigParams
          { ogmiosConfig: defaultOgmiosWsConfig
          , datumCacheConfig: defaultDatumCacheWsConfig
          , ctlServerConfig: defaultServerConfig
          , networkId: TestnetId
          , logLevel: Trace
          , extraConfig: {}
          , wallet: Just wallet
          }

        runContract_ cfg do
          logInfo' "Running Examples.Pkh2PkhKeyWallet"

          let
            constraints :: Constraints.TxConstraints Void Void
            constraints = Constraints.mustPayToPubKey (wrap (wrap pkh))
              $ Value.lovelaceValueOf lovelace

            lookups :: Lookups.ScriptLookups Void
            lookups = mempty

          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          bsTx <-
            liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
          txId <- submit bsTx
          logInfo' $ "Tx ID: " <> show txId
          liftEffect unlock
