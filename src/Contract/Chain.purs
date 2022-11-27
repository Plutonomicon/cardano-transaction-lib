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

import Contract.Monad (Contract)
import Ctl.Internal.Contract (getChainTip) as Contract
import Ctl.Internal.Contract.WaitUntilSlot
  ( currentSlot
  , currentTime
  , waitNSlots
  , waitUntilSlot
  ) as Contract
import Ctl.Internal.Serialization.Address (Slot)
import Ctl.Internal.Types.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(Tip, TipAtGenesis)
  ) as Chain
import Ctl.Internal.Types.Interval (POSIXTime)
import Ctl.Internal.Types.Natural (Natural)

getTip :: Contract Chain.Tip
getTip = Contract.getChainTip

waitUntilSlot :: Slot -> Contract Chain.Tip
waitUntilSlot = Contract.waitUntilSlot

waitNSlots :: Natural -> Contract Chain.Tip
waitNSlots = Contract.waitNSlots

currentTime :: Contract POSIXTime
currentTime = Contract.currentTime

currentSlot :: Contract Slot
currentSlot = Contract.currentSlot
