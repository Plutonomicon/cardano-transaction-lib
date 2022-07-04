-- | A module for Chain-related querying.
module Contract.Chain
  ( getTip
  , waitUntilSlot
  , module Chain
  ) where

import Prelude

import Contract.Monad (Contract, wrapContract)
import QueryM (getChainTip) as QueryM
import QueryM.WaitUntilSlot (waitUntilSlot) as QueryM
import Serialization.Address (Slot)
import Types.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(Tip, TipAtGenesis)
  ) as Chain

getTip :: forall (r :: Row Type). Contract r Chain.Tip
getTip = wrapContract QueryM.getChainTip

waitUntilSlot :: forall (r :: Row Type). Slot -> Contract r Chain.Tip
waitUntilSlot = wrapContract <<< QueryM.waitUntilSlot
