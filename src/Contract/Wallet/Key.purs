-- | Load `PrivateKey` from file or from bytes. `PrivateKey` can be used as
-- | wallet - see `Contract.Wallet`.
module Contract.Wallet.Key
  ( privateKeyFromFile
  , module X
  ) where

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Serialization.Types (PrivateKey)
import Wallet.Key (privateKeyFromFile) as Key
import Wallet.Key (privateKeyFromNormalBytes) as X

privateKeyFromFile
  :: FilePath -> Aff (Maybe PrivateKey)
privateKeyFromFile = Key.privateKeyFromFile
