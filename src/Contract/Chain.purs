-- | A module for Chain-related querying.
module CTL.Contract.Chain
  ( getTip
  , waitUntilSlot
  , waitNSlots
  , currentTime
  , currentSlot
  , module Chain
  ) where

import Prelude

import CTL.Contract.Monad (Contract, wrapContract)
import CTL.Internal.QueryM (getChainTip) as QueryM
import CTL.Internal.QueryM.WaitUntilSlot
  ( currentSlot
  , currentTime
  , waitNSlots
  , waitUntilSlot
  ) as QueryM
import CTL.Internal.Serialization.Address (Slot)
import CTL.Internal.Types.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(Tip, TipAtGenesis)
  ) as Chain
import CTL.Internal.Types.Natural (Natural)
import CTL.Internal.Types.Interval (POSIXTime)

getTip :: forall (r :: Row Type). Contract r Chain.Tip
getTip = wrapContract QueryM.getChainTip

waitUntilSlot :: forall (r :: Row Type). Slot -> Contract r Chain.Tip
waitUntilSlot = wrapContract <<< QueryM.waitUntilSlot

waitNSlots :: forall (r :: Row Type). Natural -> Contract r Chain.Tip
waitNSlots = wrapContract <<< QueryM.waitNSlots

currentTime :: forall (r :: Row Type). Contract r POSIXTime
currentTime = wrapContract QueryM.currentTime

currentSlot :: forall (r :: Row Type). Contract r Slot
currentSlot = wrapContract QueryM.currentSlot
