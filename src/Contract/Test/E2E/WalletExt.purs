module Contract.Test.E2E.WalletExt
  ( SomeWallet(SomeWallet)
  , WalletExt(FlintExt, GeroExt, LodeExt, NamiExt)
  , WalletConfig(WalletConfig)
  , module X
  , getWalletByName
  , getWalletByType
  , walletName
  ) where

import Prelude

import Contract.Test.E2E.Helpers
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
import Contract.Test.E2E.Helpers (ExtensionId(ExtensionId)) as X
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)
import Node.Path (FilePath)

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

getWalletByName :: String -> Maybe SomeWallet
getWalletByName = case _ of
  "flint" -> Just $ getWalletByType FlintExt
  "gero" -> Just $ getWalletByType GeroExt
  "lode" -> Just $ getWalletByType LodeExt
  "nami" -> Just $ getWalletByType NamiExt
  _ -> Nothing

getWalletByType :: WalletExt -> SomeWallet
getWalletByType = case _ of
  FlintExt ->
    wrap
      { wallet: FlintExt
      , name: "flint"
      , id: ExtensionId "hnhobjmcibchnmglfbldbfabcgaknlkj"
      , confirmAccess: flintConfirmAccess
      , sign: flintSign
      }
  GeroExt ->
    wrap
      { wallet: GeroExt
      , name: "gero"
      , id: ExtensionId "iifeegfcfhlhhnilhfoeihllenamcfgc"
      , confirmAccess: geroConfirmAccess
      , sign: geroSign
      }
  LodeExt ->
    wrap
      { wallet: LodeExt
      , name: "lode"
      , id: ExtensionId "aoeinhkbhhfdlfdglbkbofhigianohdm"
      , confirmAccess: lodeConfirmAccess
      , sign: lodeSign
      }
  NamiExt ->
    wrap
      { wallet: NamiExt
      , name: "nami"
      , id: ExtensionId "lpfcbjknijpeeillifnkikgncikgfhdo"
      , confirmAccess: namiConfirmAccess
      , sign: namiSign
      }

walletName :: WalletExt -> String
walletName = _.name <<< unwrap <<< getWalletByType

data WalletConfig = WalletConfig FilePath WalletPassword
