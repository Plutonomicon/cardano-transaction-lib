-- | A module for Chain-related querying.
module Contract.Chain
  ( module Chain
  , module Contract
  , module Contract.WaitUntilSlot
  ) where

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
