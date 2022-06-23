-- | A module for Chain-related querying.
module Contract.Chain
  ( getTip
  , waitUntilSlot
  , module Chain
  ) where

import Prelude

import Contract.Monad (Contract, wrapContract)
import QueryM (getChainTip, waitUntilSlot) as QueryM
import QueryM.Ogmios (AbsSlot)
import Types.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(Tip, TipAtGenesis)
  ) as Chain

getTip :: forall (r :: Row Type). Contract r Chain.Tip
getTip = wrapContract QueryM.getChainTip

waitUntilSlot :: forall (r :: Row Type). AbsSlot -> Contract r Chain.Tip
waitUntilSlot = wrapContract <<< QueryM.waitUntilSlot
