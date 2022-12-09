-- | A module for Chain-related querying.
module Contract.Chain
  ( getTip
  , module Chain
  , module Contract.WaitUntilSlot
  ) where

import Contract.Monad (Contract)
import Ctl.Internal.Contract (getChainTip) as Contract
import Ctl.Internal.Contract.WaitUntilSlot
  ( currentSlot
  , currentTime
  , waitNSlots
  , waitUntilSlot
  ) as Contract.WaitUntilSlot
import Ctl.Internal.Types.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(Tip, TipAtGenesis)
  ) as Chain

getTip :: Contract Chain.Tip
getTip = Contract.getChainTip
