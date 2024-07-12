module Test.Ctl.Testnet.Common
  ( privateStakeKey
  ) where

import Prelude

import Cardano.Wallet.Key (PrivateStakeKey)
import Contract.Keys (privateKeyFromBytes)
import Data.ByteArray (hexToByteArray)
import Data.Maybe (fromJust)
import Data.Newtype (wrap)
import Partial.Unsafe (unsafePartial)

privateStakeKey :: PrivateStakeKey
privateStakeKey = wrap $ unsafePartial $ fromJust
  $ privateKeyFromBytes =<< map wrap
      ( hexToByteArray
          "633b1c4c4a075a538d37e062c1ed0706d3f0a94b013708e8f5ab0a0ca1df163d"
      )
