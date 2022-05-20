-- | A module containing time-related datatypes and helpers.
module Contract.Time (module SerializationAddress, module Interval) where

import Serialization.Address
  ( Slot(Slot)
  , BlockId(BlockId)
  ) as SerializationAddress
import Types.Interval
  ( Closure
  , Extended(NegInf, Finite, PosInf)
  , Interval(Interval)
  , LowerBound(LowerBound)
  , POSIXTime(POSIXTime)
  , POSIXTimeRange
  , SlotConfig(SlotConfig)
  , SlotRange
  , UpperBound(UpperBound)
  , after
  , always
  , before
  , beginningOfTime
  , contains
  , defaultSlotConfig
  , from
  , hull
  , intersection
  , interval
  , isEmpty
  -- , isEmpty'
  , lowerBound
  , member
  , mkInterval
  , never
  , overlaps
  -- , overlaps'
  , posixTimeRangeToContainedSlotRange
  , posixTimeRangeToTransactionSlot
  , posixTimeToEnclosingSlot
  , singleton
  , slotRangeToPOSIXTimeRange
  , slotRangeToTransactionSlot
  , slotToEndPOSIXTime
  , slotToPOSIXTimeRange
  , strictLowerBound
  , strictUpperBound
  , to
  , upperBound
  ) as Interval
