-- | A module for Chain-related querying.
module Contract.Chain
  ( getTip
  , module X
  ) where

import Contract.Monad (Contract)
import Ctl.Internal.Contract (getChainTip)
import Ctl.Internal.Contract.WaitUntilSlot
  ( currentSlot
  , currentTime
  , waitNSlots
  , waitUntilSlot
  ) as X
import Ctl.Internal.Types.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(Tip, TipAtGenesis)
  ) as X
import Ctl.Internal.Types.Chain (Tip)

getTip :: Contract Tip
getTip = getChainTip
