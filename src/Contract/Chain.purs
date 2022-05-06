-- | A module for Chain-related querying.
module Contract.Chain
  ( getTip
  , module Chain
  ) where

import Contract.Monad (Contract, wrapContract)
import QueryM (getChainTip) as QueryM
import Types.Chain (BlockHeaderHash(..), Tip(..)) as Chain

getTip :: forall (r :: Row Type). Contract r Chain.Tip
getTip = wrapContract QueryM.getChainTip
