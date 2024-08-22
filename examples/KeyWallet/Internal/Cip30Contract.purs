module Ctl.Examples.KeyWallet.Internal.Cip30Contract
  ( runKeyWalletContract_
  ) where

import Contract.Prelude

import Cardano.Types (RawBytes)
import Cardano.Types.PrivateKey as PrivateKey
import Contract.Config
  ( PrivatePaymentKeySource(PrivatePaymentKeyValue)
  , WalletSpec(UseKeys)
  , testnetConfig
  )
import Contract.Monad (Contract, launchAff_, runContract)
import Control.Monad.Error.Class (class MonadError, catchError, liftMaybe)
import Ctl.Examples.KeyWallet.Internal.Cip30HtmlForm (Log, Unlock)
import Ctl.Examples.KeyWallet.Internal.Cip30HtmlForm
  ( levelColor
  , levelName
  , logError
  , mkForm
  ) as HtmlForm
import Data.ByteArray (hexToByteArray)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error, message)

runKeyWalletContract_
  :: (RawBytes -> Contract Unit) -> Effect Unit
runKeyWalletContract_ contract =
  HtmlForm.mkForm \input log' unlock ->
    launchAff_ $ flip catchError (errorHandler log' unlock) $ do
      privateKey <- liftMaybe (error "Failed to parse private key")
        $ PrivateKey.fromRawBytes
        =<< map wrap (hexToByteArray input.privateKey)
      message <- liftMaybe (error "Failed to encode message")
        $ map wrap
        $ hexToByteArray input.message
      let
        cfg = testnetConfig
          { walletSpec = Just $ UseKeys
              (PrivatePaymentKeyValue $ wrap privateKey)
              Nothing
              Nothing
          , customLogger = Just printLog
          }

        printLog :: LogLevel -> Message -> Aff Unit
        printLog level m = liftEffect $ when (m.level >= level) $ do
          prettyFormatter m >>= log
          log' (HtmlForm.levelColor m.level)
            ("[" <> HtmlForm.levelName m.level <> "] " <> m.message)
      runContract cfg (contract message)
  where

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
