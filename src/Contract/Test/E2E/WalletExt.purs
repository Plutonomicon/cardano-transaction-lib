module CTL.Contract.Test.E2E.WalletExt
  ( SomeWallet(SomeWallet)
  , WalletExt(FlintExt, GeroExt, LodeExt, NamiExt)
  , WalletConfig(WalletConfig)
  , module X
  , getWalletByName
  , getWalletByType
  , walletName
  ) where

import Prelude

import CTL.Contract.Test.E2E.Helpers (ExtensionId(ExtensionId)) as X
import CTL.Contract.Test.E2E.Helpers
  ( ExtensionId(ExtensionId)
  , RunningExample
  , WalletPassword
  , flintConfirmAccess
  , flintSign
  , geroConfirmAccess
  , geroSign
  , lodeConfirmAccess
  , lodeSign
  , namiConfirmAccess
  , namiSign
  )
import Control.Monad.Error.Class (liftMaybe)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import Record as Record

data WalletExt = FlintExt | NamiExt | GeroExt | LodeExt

derive instance Eq WalletExt
derive instance Ord WalletExt

newtype SomeWallet = SomeWallet
  { wallet :: WalletExt
  , name :: String
  , id :: ExtensionId
  , confirmAccess :: ExtensionId -> RunningExample -> Aff Unit
  , sign :: ExtensionId -> WalletPassword -> RunningExample -> Aff Unit
  }

derive instance Newtype SomeWallet _

getWalletByName :: String -> Effect (Maybe SomeWallet)
getWalletByName = case _ of
  "flint" -> Just <$> getWalletByType FlintExt
  "gero" -> Just <$> getWalletByType GeroExt
  "lode" -> Just <$> getWalletByType LodeExt
  "nami" -> Just <$> getWalletByType NamiExt
  _ -> pure Nothing

readExtIdEnvVar :: String -> Effect ExtensionId
readExtIdEnvVar varName = do
  mbStr <- lookupEnv varName
  ExtensionId <$> liftMaybe
    (error $ "Unable to get " <> varName <> " environment variable")
    mbStr

getWalletByType :: WalletExt -> Effect SomeWallet
getWalletByType walletExt = addName case walletExt of
  FlintExt -> do
    id <- readExtIdEnvVar "FLINT_EXTID"
    pure
      { wallet: FlintExt
      , id
      , confirmAccess: flintConfirmAccess
      , sign: flintSign
      }
  GeroExt -> do
    id <- readExtIdEnvVar "GERO_EXTID"
    pure
      { wallet: GeroExt
      , id
      , confirmAccess: geroConfirmAccess
      , sign: geroSign
      }
  LodeExt -> do
    id <- readExtIdEnvVar "LODE_EXTID"
    pure
      { wallet: LodeExt
      , id
      , confirmAccess: lodeConfirmAccess
      , sign: lodeSign
      }
  NamiExt -> do
    id <- readExtIdEnvVar "NAMI_EXTID"
    pure
      { wallet: NamiExt
      , id
      , confirmAccess: namiConfirmAccess
      , sign: namiSign
      }
  where
  addName m = m <#> Record.merge { name: walletName walletExt } >>> wrap

walletName :: WalletExt -> String
walletName = case _ of
  GeroExt -> "gero"
  LodeExt -> "lode"
  NamiExt -> "namie"
  FlintExt -> "flint"

data WalletConfig = WalletConfig FilePath WalletPassword
