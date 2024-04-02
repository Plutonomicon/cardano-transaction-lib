module Ctl.Internal.Types.EraSummaries
  ( EpochLength(EpochLength)
  , EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  , EraSummaryTime(EraSummaryTime)
  , RelativeTime(RelativeTime)
  , SafeZone(SafeZone)
  , SlotLength(SlotLength)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , encodeAeson
  , finiteNumber
  , getField
  , (.:)
  )
import Control.Alt ((<|>))
import Ctl.Internal.Helpers (showWithParens)
import Ctl.Internal.Serialization.Address (Slot)
import Ctl.Internal.Service.Helpers (aesonObject)
import Ctl.Internal.Types.Epoch (Epoch)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import JS.BigInt (BigInt)
import Partial.Unsafe (unsafePartial)

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
  { time :: RelativeTime
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

-- | A time in seconds relative to another one (typically, system start or era
-- | start).
newtype RelativeTime = RelativeTime Number

derive instance Generic RelativeTime _
derive instance Newtype RelativeTime _
derive newtype instance Eq RelativeTime
derive newtype instance Ord RelativeTime

instance DecodeAeson RelativeTime where
  decodeAeson aeson = decodeV5 <|> decodeV6
    where
    decodeV5 = RelativeTime <$> decodeAeson aeson
    decodeV6 = do
      obj <- decodeAeson aeson
      RelativeTime <$> obj .: "seconds"

instance EncodeAeson RelativeTime where
  encodeAeson (RelativeTime rt) =
    -- We assume the numbers are finite.
    encodeAeson $ unsafePartial $ fromJust $ finiteNumber rt

instance Show RelativeTime where
  show (RelativeTime rt) = showWithParens "RelativeTime" rt

newtype EpochLength = EpochLength BigInt

derive instance Generic EpochLength _
derive instance Newtype EpochLength _
derive newtype instance Eq EpochLength
derive newtype instance DecodeAeson EpochLength
derive newtype instance EncodeAeson EpochLength

instance Show EpochLength where
  show (EpochLength epochLength) = showWithParens "EpochLength" epochLength

-- | A slot length, in milliseconds.
newtype SlotLength = SlotLength Number

derive instance Generic SlotLength _
derive instance Newtype SlotLength _
derive newtype instance Eq SlotLength
derive newtype instance DecodeAeson SlotLength

instance EncodeAeson SlotLength where
  encodeAeson (SlotLength sl) =
    -- We assume the numbers are finite.
    encodeAeson $ unsafePartial $ fromJust $ finiteNumber sl

instance Show SlotLength where
  show (SlotLength slotLength) = showWithParens "SlotLength" slotLength

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
