module Wallet
  ( module KeyWallet
  , module Cip30Wallet
  , Wallet(Gero, Nami, KeyWallet, KeyListWallet)
  , mkNamiWalletAff
  , mkGeroWalletAff
  , mkKeyWallet
  , cip30Wallet
  , dummySign
  ) where

import Prelude

import Cardano.Types.Transaction
  ( Ed25519Signature(Ed25519Signature)
  , PublicKey(PublicKey)
  , Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  )
import Control.Promise (Promise)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (over)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Helpers ((<<>>))
import Wallet.Cip30 (Cip30Wallet, Cip30Connection) as Cip30Wallet
import Wallet.Cip30 (Cip30Wallet, Cip30Connection, mkCip30WalletAff)
import Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )
import Wallet.Key (KeyWallet, privateKeysToKeyWallet) as KeyWallet
import Wallet.KeyList (KeyListWallet)

data Wallet
  = Nami Cip30Wallet
  | Gero Cip30Wallet
  | KeyWallet KeyWallet
  | KeyListWallet KeyListWallet

mkKeyWallet :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Wallet
mkKeyWallet payKey mbStakeKey = KeyWallet $ privateKeysToKeyWallet payKey
  mbStakeKey

mkNamiWalletAff :: Aff Wallet
mkNamiWalletAff = Nami <$> mkCip30WalletAff "Nami" _enableNami

foreign import _enableNami :: Effect (Promise Cip30Connection)

mkGeroWalletAff :: Aff Wallet
mkGeroWalletAff = Gero <$> mkCip30WalletAff "Gero" _enableGero

foreign import _enableGero :: Effect (Promise Cip30Connection)

cip30Wallet :: Wallet -> Maybe Cip30Wallet
cip30Wallet = case _ of
  Nami c30 -> Just c30
  Gero c30 -> Just c30
  _ -> Nothing

-- Attach a dummy vkey witness to a transaction. Helpful for when we need to
-- know the number of witnesses (e.g. fee calculation) but the wallet hasn't
-- signed (or cannot sign) yet
dummySign :: Transaction -> Transaction
dummySign tx@(Transaction { witnessSet: tws@(TransactionWitnessSet ws) }) =
  over Transaction _
    { witnessSet = over TransactionWitnessSet
        _
          { vkeys = ws.vkeys <<>> Just [ vk ]
          }
        tws
    }
    $ tx
  where
  vk :: Vkeywitness
  vk = Vkeywitness
    ( Vkey
        ( PublicKey
            "ed25519_pk1eamrnx3pph58yr5l4z2wghjpu2dt2f0rp0zq9qquqa39p52ct0xsudjp4e"
        )
        /\ Ed25519Signature
          "ed25519_sig1ynufn5umzl746ekpjtzt2rf58ep0wg6mxpgyezh8vx0e8jpgm3kuu3tgm453wlz4rq5yjtth0fnj0ltxctaue0dgc2hwmysr9jvhjzswt86uk"
    )
