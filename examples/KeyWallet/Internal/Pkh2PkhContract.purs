module Ctl.Examples.KeyWallet.Internal.Pkh2PkhContract
  ( runKeyWalletContract_
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Config
  ( PrivatePaymentKeySource(PrivatePaymentKeyValue)
  , WalletSpec(UseKeys)
  , testnetConfig
  )
import Contract.Monad (Contract, launchAff_, runContract)
import Control.Monad.Error.Class (class MonadError, catchError, liftMaybe)
import Ctl.Examples.KeyWallet.Internal.Pkh2PkhHtmlForm (Log, Unlock)
import Ctl.Examples.KeyWallet.Internal.Pkh2PkhHtmlForm
  ( levelColor
  , levelName
  , logError
  , mkForm
  ) as HtmlForm
import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashFromBech32)
import Ctl.Internal.Types.RawBytes (hexToRawBytes)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error, message)
import JS.BigInt (BigInt)
import JS.BigInt (fromString) as BigInt

runKeyWalletContract_
  :: (PaymentPubKeyHash -> BigInt -> Unlock -> Contract Unit) -> Effect Unit
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
      let
        cfg = testnetConfig
          { walletSpec = Just $ UseKeys
              (PrivatePaymentKeyValue $ wrap privateKey)
              Nothing
          , customLogger = Just printLog
          }

        printLog :: LogLevel -> Message -> Aff Unit
        printLog lgl m = liftEffect $ when (m.level >= lgl) $ do
          prettyFormatter m >>= log
          log' (HtmlForm.levelColor m.level)
            ("[" <> HtmlForm.levelName m.level <> "] " <> m.message)
      runContract cfg (contract pkh lovelace unlock)
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
