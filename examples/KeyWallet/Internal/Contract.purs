module Ctl.Examples.KeyWallet.Internal.Contract
  ( runKeyWalletContract
  ) where

import Contract.Prelude

import Contract.Config
  ( MnemonicSource(MnemonicString)
  , StakeKeyPresence(WithStakeKey)
  , WalletSpec(UseMnemonic)
  , testnetConfig
  )
import Contract.Monad (Contract, launchAff_, runContract)
import Control.Monad.Error.Class (class MonadError, catchError)
import Ctl.Examples.KeyWallet.Internal.HtmlForm (Log, Unlock)
import Ctl.Examples.KeyWallet.Internal.HtmlForm
  ( levelColor
  , levelName
  , logError
  , mkForm
  ) as HtmlForm
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, message)

runKeyWalletContract :: (Unlock -> Contract Unit) -> Effect Unit
runKeyWalletContract contract =
  HtmlForm.mkForm \input log' unlock ->
    launchAff_ $ flip catchError (errorHandler log' unlock) $ do
      let
        cfg = testnetConfig
          { walletSpec = Just $ UseMnemonic
              (MnemonicString input.walletSeed)
              { accountIndex: zero, addressIndex: zero }
              WithStakeKey
          , customLogger = Just printLog
          }

        printLog :: LogLevel -> Message -> Aff Unit
        printLog lgl m = liftEffect $ when (m.level >= lgl) $ do
          prettyFormatter m >>= log
          log' (HtmlForm.levelColor m.level)
            ("[" <> HtmlForm.levelName m.level <> "] " <> m.message)
      runContract cfg (contract unlock)
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
