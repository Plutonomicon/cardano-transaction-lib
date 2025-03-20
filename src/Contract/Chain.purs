-- | A module for Chain-related querying.
module Contract.Chain
  ( getTip
  , module Chain
  , module Contract.WaitUntilSlot
  ) where

import Cardano.Types.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(Tip, TipAtGenesis)
  ) as Chain
import Contract.Monad (Contract)
import Ctl.Internal.Contract (getChainTip) as Contract
import Ctl.Internal.Contract.WaitUntilSlot
  ( currentSlot
  , currentTime
  , waitNSlots
  , waitUntilSlot
  ) as Contract.WaitUntilSlot

getTip :: Contract Chain.Tip
getTip = Contract.getChainTip
