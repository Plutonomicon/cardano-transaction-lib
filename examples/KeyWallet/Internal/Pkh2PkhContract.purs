module Examples.KeyWallet.Internal.Pkh2PkhContract
  ( runKeyWalletContract_
  ) where

import Contract.Prelude

import Contract.Address (NetworkId(TestnetId), PaymentPubKeyHash)
import Contract.Monad
  ( ConfigParams(ConfigParams)
  , Contract
  , ContractConfig(ContractConfig)
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , launchAff_
  , mkContractConfig
  )
import Control.Monad.Error.Class (class MonadError, catchError, liftMaybe)
import Control.Monad.Logger.Trans (runLoggerT)
import Control.Monad.Reader (runReaderT)
import Data.BigInt (BigInt)
import Data.BigInt (fromString) as BigInt
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Level (LogLevel(Trace))
import Data.Log.Message (Message)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error, message)
import Examples.KeyWallet.Internal.Pkh2PkhHtmlForm (Log, Unlock)
import Examples.KeyWallet.Internal.Pkh2PkhHtmlForm
  ( mkForm
  , levelColor
  , levelName
  , logError
  ) as HtmlForm
import Serialization (privateKeyFromBytes)
import Serialization.Hash (ed25519KeyHashFromBech32)
import Types.RawBytes (hexToRawBytes)
import Wallet (mkKeyWallet)

runKeyWalletContract_
  :: (PaymentPubKeyHash -> BigInt -> Unlock -> Contract () Unit) -> Effect Unit
runKeyWalletContract_ contract =
  HtmlForm.mkForm \input log' unlock ->
    launchAff_ $ flip catchError (errorHandler log' unlock) $ do
      privateKey <- liftMaybe (error "Failed to parse private key")
        $ privateKeyFromBytes
        =<< hexToRawBytes input.privateKey
      pkh <- liftMaybe (error "Failed to parse public key hash")
        $ map (wrap <<< wrap)
        $ ed25519KeyHashFromBech32 input.toPkh
      lovelace <- liftMaybe (error "Failed to parse lovelace amount") $
        BigInt.fromString input.lovelace
      let wallet = mkKeyWallet (wrap privateKey) Nothing
      cfg <- mkContractConfig $ ConfigParams
        { ogmiosConfig: defaultOgmiosWsConfig
        , datumCacheConfig: defaultDatumCacheWsConfig
        , ctlServerConfig: defaultServerConfig
        , networkId: TestnetId
        , logLevel: Trace
        , extraConfig: {}
        , wallet: Just wallet
        }
      runContract' log' cfg (contract pkh lovelace unlock)
  where
  runContract'
    :: forall (r :: Row Type)
     . Log
    -> ContractConfig r
    -> Contract r Unit
    -> Aff Unit
  runContract' log' (ContractConfig config) =
    flip runLoggerT printLog <<< flip runReaderT config <<< unwrap
    where
    printLog :: Message -> Aff Unit
    printLog m = liftEffect $ when (m.level >= config.logLevel) $ do
      prettyFormatter m >>= log
      log' (HtmlForm.levelColor m.level)
        ("[" <> HtmlForm.levelName m.level <> "] " <> m.message)

  errorHandler
    :: forall (m :: Type -> Type)
     . MonadError Error m
    => MonadEffect m
    => Log
    -> Unlock
    -> Error
    -> m Unit
  errorHandler log' unlock e =
    liftEffect $ HtmlForm.logError e
      *> log' "crimson" ("[ERROR] " <> message e)
      *> unlock
