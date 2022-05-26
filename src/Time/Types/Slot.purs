module Time.Types.Slot
  ( SlotConfig(..)
  , SlotRange
  , beginningOfTime
  , defaultSlotConfig
  , module Slot
  ) where

import Data.BigInt (BigInt)
import Data.BigInt (fromString, fromInt) as BigInt
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Prelude
import Serialization.Address (Slot(Slot)) as Slot
import Time.Types.Interval (Interval)
import Time.Types.POSIXTime (POSIXTime(POSIXTime))

--------------------------------------------------------------------------------
-- SlotConfig Type and related
--------------------------------------------------------------------------------
-- Most of these functions could use a Reader constraint over `SlotConfig` but
-- that would depend on how the `Contract` monad pans out, we'll keep it
-- explicit for now.

type SlotRange = Interval Slot.Slot

newtype SlotConfig = SlotConfig
  { slotLength :: BigInt -- Length (number of milliseconds) of one slot
  , slotZeroTime :: POSIXTime -- Beginning of slot 0 (in milliseconds)
  }

derive instance Generic SlotConfig _
derive newtype instance Eq SlotConfig

instance Show SlotConfig where
  show = genericShow

-- | 'beginningOfTime' corresponds to the Shelley launch date
-- | (2020-07-29T21:44:51Z) which is 1596059091000 in POSIX time
-- | (number of milliseconds since 1970-01-01T00:00:00Z).
beginningOfTime :: BigInt
beginningOfTime = unsafePartial fromJust $ BigInt.fromString "1596059091000"

defaultSlotConfig :: SlotConfig
defaultSlotConfig = SlotConfig
  { slotLength: BigInt.fromInt 1000 -- One second = 1 slot currently.
  , slotZeroTime: POSIXTime beginningOfTime
  }
