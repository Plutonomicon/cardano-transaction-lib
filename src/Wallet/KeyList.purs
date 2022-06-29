module Wallet.KeyList
  ( KeyListWallet
  , mkKeyListWallet
  ) where

import Prelude
import Data.Maybe (Maybe(Nothing))
import Wallet.Key (KeyWallet, privateKeyToKeyWallet)
import Serialization.Types (PrivateKey)

type KeyListWallet =
  { selected :: Maybe KeyWallet
  , available :: Array KeyWallet
  }

mkKeyListWallet :: Array PrivateKey -> KeyListWallet
mkKeyListWallet pks =
  { selected: Nothing
  , available: pks <#> privateKeyToKeyWallet
  }
