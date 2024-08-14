module Ctl.Examples.Gov.Internal.Common
  ( asRewardAddress
  , dummyAnchor
  ) where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types
  ( Address(RewardAddress)
  , Anchor(Anchor)
  , RewardAddress
  , URL(URL)
  )
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Partial.Unsafe (unsafePartial)

asRewardAddress :: Address -> Maybe RewardAddress
asRewardAddress = case _ of
  RewardAddress rewardAddr -> Just rewardAddr
  _ -> Nothing

dummyAnchor :: Anchor
dummyAnchor =
  Anchor
    { url: URL "https://example.com/"
    , dataHash:
        unsafePartial $ fromJust $ decodeCbor $ wrap $
          hexToByteArrayUnsafe
            "94b8cac47761c1140c57a48d56ab15d27a842abff041b3798b8618fa84641f5a"
    }
