module Wallet.KeyList
  ( KeyListWallet
  , mkKeyListWallet
  ) where

import Prelude
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (wrap)
import Serialization.Types (PrivateKey)
import Wallet.Key (KeyWallet, privateKeysToKeyWallet)

type KeyListWallet =
  { selected :: Maybe KeyWallet
  , available :: Array KeyWallet
  }

mkKeyListWallet :: Array PrivateKey -> KeyListWallet
mkKeyListWallet pks =
  { selected: Nothing
  , available: pks <#> wrap >>> flip privateKeysToKeyWallet Nothing
  -- TODO: add stake keys to plutip:
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/659
  }
