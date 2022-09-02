module Contract.Test.E2E.WalletExt
  ( SomeWallet(SomeWallet)
  , WalletExt(FlintExt, GeroExt, LodeExt, NamiExt)
  , WalletConfig(WalletConfig)
  , getWalletByName
  , getWalletByType
  , walletName
  ) where

import Prelude

import Contract.Test.E2E.Helpers
  ( RunningExample
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

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Map (Map)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(Tuple), snd)
import Effect.Aff (Aff)
import Node.Path (FilePath)

data WalletExt = FlintExt | NamiExt | GeroExt | LodeExt

derive instance Eq WalletExt
derive instance Ord WalletExt

newtype SomeWallet = SomeWallet
  { wallet :: WalletExt
  , name :: String
  , id :: String
  , confirmAccess :: RunningExample -> Aff Unit
  , sign :: WalletPassword -> RunningExample -> Aff Unit
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
      , id: "hnhobjmcibchnmglfbldbfabcgaknlkj"
      , confirmAccess: flintConfirmAccess
      , sign: flintSign
      }
  GeroExt ->
    wrap
      { wallet: GeroExt
      , name: "gero"
      , id: "iifeegfcfhlhhnilhfoeihllenamcfgc"
      , confirmAccess: geroConfirmAccess
      , sign: geroSign
      }
  LodeExt ->
    wrap
      { wallet: LodeExt
      , name: "lode"
      , id: "ikffplhknjhbfkgbhnionfklokakmknd"
      , confirmAccess: lodeConfirmAccess
      , sign: lodeSign
      }
  NamiExt ->
    wrap
      { wallet: NamiExt
      , name: "nami"
      , id: "lpfcbjknijpeeillifnkikgncikgfhdo"
      , confirmAccess: namiConfirmAccess
      , sign: namiSign
      }

walletName :: WalletExt -> String
walletName = _.name <<< unwrap <<< getWalletByType

data WalletConfig = WalletConfig FilePath WalletPassword


