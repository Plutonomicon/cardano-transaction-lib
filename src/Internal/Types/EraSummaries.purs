module Ctl.Internal.Types.EraSummaries
  ( EpochLength(EpochLength)
  , EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  , EraSummaryTime(EraSummaryTime)
  , Seconds(Seconds)
  , fromMilliseconds
  , Milliseconds(Milliseconds)
  , fromSeconds
  , SafeZone(SafeZone)
  , SlotLength
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , encodeAeson
  , getField
  )
import Ctl.Internal.Helpers (showWithParens)
import Ctl.Internal.Serialization.Address (Slot)
import Ctl.Internal.Service.Helpers (aesonObject)
import Ctl.Internal.Types.Epoch (Epoch)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import JS.BigInt (BigInt, fromInt)

--------------------------------------------------------------------------------
-- EraSummaries
--------------------------------------------------------------------------------

newtype EraSummaries = EraSummaries (Array EraSummary)

derive instance Generic EraSummaries _
derive instance Newtype EraSummaries _
derive instance Eq EraSummaries

instance Show EraSummaries where
  show = genericShow

--------------------------------------------------------------------------------
-- EraSummary
--------------------------------------------------------------------------------

-- | start: Era bound which captures the time, slot and epoch at which the
-- | era starts, relative to the start of the network.
-- |
-- | end: Era bound which captures the time, slot and epoch at which the
-- | era ends, relative to the start of the network.
-- |
-- | parameters: Era parameters that can vary across hard forks.
newtype EraSummary = EraSummary
  { start :: EraSummaryTime
  , end :: Maybe EraSummaryTime
  , parameters :: EraSummaryParameters
  }

derive instance Generic EraSummary _
derive instance Newtype EraSummary _
derive newtype instance Eq EraSummary

instance Show EraSummary where
  show = genericShow

--------------------------------------------------------------------------------
-- EraSummaryTime
--------------------------------------------------------------------------------

newtype Seconds = Seconds BigInt

derive instance Newtype Seconds _
derive newtype instance Show Seconds
derive newtype instance Eq Seconds

instance EncodeAeson Seconds where
  encodeAeson (Seconds seconds) = encodeAeson { seconds }

instance DecodeAeson Seconds where
  decodeAeson = decodeAeson
    >=> flip getField "seconds"
    >=> pure <<< wrap

fromMilliseconds :: Milliseconds -> Seconds
fromMilliseconds (Milliseconds n) = Seconds $ n / fromInt 1000

newtype Milliseconds = Milliseconds BigInt

derive instance Newtype Milliseconds _
derive newtype instance Show Milliseconds
derive newtype instance Eq Milliseconds

instance EncodeAeson Milliseconds where
  encodeAeson (Milliseconds milliseconds) = encodeAeson { milliseconds }

instance DecodeAeson Milliseconds where
  decodeAeson = decodeAeson
    >=> flip getField "milliseconds"
    >=> pure <<< wrap

fromSeconds :: Seconds -> Milliseconds
fromSeconds (Seconds n) = Milliseconds $ n * fromInt 1000

-- | time: Time in seconds relative to the start time of the network.
-- |
-- | slot: Absolute slot number.
-- | Ogmios returns a number 0-18446744073709552000 but our `Slot` is a Rust u64
-- | which has precision up to 18446744073709551615 (note 385 difference).
-- | We treat this as neglible instead of defining `AbsSlot BigInt`.
-- | See https://github.com/Plutonomicon/cardano-transaction-lib/issues/632
-- | for details.
-- |
-- | epoch: Epoch number.
newtype EraSummaryTime = EraSummaryTime
  { time :: Seconds
  , slot :: Slot
  , epoch :: Epoch
  }

derive instance Generic EraSummaryTime _
derive instance Newtype EraSummaryTime _
derive newtype instance Eq EraSummaryTime

instance Show EraSummaryTime where
  show = genericShow

instance DecodeAeson EraSummaryTime where
  decodeAeson = aesonObject \obj -> do
    time <- getField obj "time"
    slot <- getField obj "slot"
    epoch <- getField obj "epoch"
    pure $ wrap { time, slot, epoch }

instance EncodeAeson EraSummaryTime where
  encodeAeson (EraSummaryTime { time, slot, epoch }) =
    encodeAeson { "time": time, "slot": slot, "epoch": epoch }

--------------------------------------------------------------------------------
-- EraSummaryParameters
--------------------------------------------------------------------------------

-- | epochLength: Epoch length in number of slots.
-- |
-- | slotLength: Slot length in milliseconds.
-- |
-- | safe_zone: Zone in which it is guaranteed that no hard fork can take place.
newtype EraSummaryParameters = EraSummaryParameters
  { epochLength :: EpochLength
  , slotLength :: SlotLength
  , safeZone :: SafeZone
  }

derive instance Generic EraSummaryParameters _
derive instance Newtype EraSummaryParameters _
derive newtype instance Eq EraSummaryParameters

instance Show EraSummaryParameters where
  show = genericShow

--------------------------------------------------------------------------------
-- RelativeTime, Epoch, EpochLength, SlotLength, SafeZone
--------------------------------------------------------------------------------

newtype EpochLength = EpochLength BigInt

derive instance Generic EpochLength _
derive instance Newtype EpochLength _
derive newtype instance Eq EpochLength
derive newtype instance DecodeAeson EpochLength
derive newtype instance EncodeAeson EpochLength

instance Show EpochLength where
  show (EpochLength epochLength) = showWithParens "EpochLength" epochLength

-- | A slot length, in milliseconds.
type SlotLength = Milliseconds

-- | Number of slots from the tip of the ledger in which it is guaranteed that
-- | no hard fork can take place. This should be (at least) the number of slots
-- | in which we are guaranteed to have k blocks.
newtype SafeZone = SafeZone BigInt

derive instance Generic SafeZone _
derive instance Newtype SafeZone _
derive newtype instance Eq SafeZone
derive newtype instance Semiring SafeZone
derive newtype instance DecodeAeson SafeZone
derive newtype instance EncodeAeson SafeZone

instance Show SafeZone where
  show (SafeZone sz) = showWithParens "SafeZone" sz
