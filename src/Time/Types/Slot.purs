module Time.Types.Slot
  ( SlotRange
  , beginningOfTime
  , maxSlot
  , module Slot
  ) where

import Data.BigInt (BigInt)
import Data.BigInt (fromString) as BigInt
import Data.Maybe (fromJust)
import Data.UInt as UInt
import Partial.Unsafe (unsafePartial)
import Prelude
import Serialization.Address (Slot(Slot)) as Slot
import Time.Types.Interval (Interval)

--------------------------------------------------------------------------------
-- SlotConfig Type and related
--------------------------------------------------------------------------------
-- Most of these functions could use a Reader constraint over `SlotConfig` but
-- that would depend on how the `Contract` monad pans out, we'll keep it
-- explicit for now.

type SlotRange = Interval Slot.Slot

-- | 'beginningOfTime' corresponds to the Shelley launch date
-- | (2020-07-29T21:44:51Z) which is 1596059091000 in POSIX time
-- | (number of milliseconds since 1970-01-01T00:00:00Z).
beginningOfTime :: BigInt
beginningOfTime = unsafePartial fromJust $ BigInt.fromString "1596059091000"

-- | Maximum slot under `Data.UInt`
maxSlot :: Slot.Slot
maxSlot = Slot.Slot $ unsafePartial fromJust $ UInt.fromString "4294967295"