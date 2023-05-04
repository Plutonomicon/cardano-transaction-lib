module Test.Ctl.Plutip.Common
  ( config
  , privateStakeKey
  ) where

import Prelude

import Contract.Test.Plutip (defaultPlutipConfig)
import Contract.Wallet (privateKeyFromBytes)
import Ctl.Internal.Plutip.Types (PlutipConfig)
import Ctl.Internal.Types.RawBytes (hexToRawBytes)
import Ctl.Internal.Wallet.Key (PrivateStakeKey)
import Data.Maybe (fromJust)
import Data.Newtype (wrap)
import Data.UInt as UInt
import Partial.Unsafe (unsafePartial)

config :: PlutipConfig
config = defaultPlutipConfig { clusterConfig = defaultPlutipConfig.clusterConfig { epochSize = pure $ UInt.fromInt 0, maxTxSize = pure $ UInt.fromInt 15000, raiseExUnitsToMax = true }}

privateStakeKey :: PrivateStakeKey
privateStakeKey = wrap $ unsafePartial $ fromJust
  $ privateKeyFromBytes =<< hexToRawBytes
      "633b1c4c4a075a538d37e062c1ed0706d3f0a94b013708e8f5ab0a0ca1df163d"
