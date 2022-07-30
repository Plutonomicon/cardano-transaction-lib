-- | A module for Chain-related querying.
module Contract.Chain
  ( getTip
  , waitUntilSlot
  , waitNSlots
  , currentTime
  , currentSlot
  , module Chain
  ) where

import Prelude

import Contract.Monad (Contract, wrapContract)
import QueryM (getChainTip) as QueryM
import QueryM.WaitUntilSlot
  ( currentSlot
  , currentTime
  , waitNSlots
  , waitUntilSlot
  ) as QueryM
import Serialization.Address (Slot)
import Types.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(Tip, TipAtGenesis)
  ) as Chain
import Types.Natural (Natural)
import Types.Interval (POSIXTime)

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
