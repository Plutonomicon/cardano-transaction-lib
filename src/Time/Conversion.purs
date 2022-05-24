module Time.Conversion
  ( SlotToPOSIXTimeError(..)
  , slotToPOSIXTime
  ) where

import Prelude
import Data.BigInt (BigInt)
import Control.Alternative (guard, unless)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (find)
import Data.Generic.Rep (class Generic)
import Data.JSDate (parse, toDateTime)
import Data.Either (Either, note)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect.Class (liftEffect)
import Helpers (bigIntToUInt, liftedM, liftEither, liftM, uIntToBigInt)
import QueryM (QueryM)
import QueryM.EraSummaries (getEraSummaries)
import QueryM.Ogmios
  ( AbsSlot(AbsSlot)
  , EraSummariesQR(EraSummariesQR)
  , EraSummary(EraSummary)
  , EraSummaryTime(EraSummaryTime)
  , SystemStartQR
  )
import QueryM.SystemStart (getSystemStart)
import Serialization.Address (Slot)
import Types.Interval (POSIXTime)
import Undefined (undefined)

data SlotToPOSIXTimeError
  = ParseToDataTime SystemStartQR
  | CannotFindSlotInEraSummaries AbsSlot
  | StartingSlotGreaterThanSlot AbsSlot
  | SlotFromAbsSlot AbsSlot -- perhaps remove

derive instance Generic SlotToPOSIXTimeError _
derive instance Eq SlotToPOSIXTimeError

instance Show SlotToPOSIXTimeError where
  show = genericShow

-- | Converts a CSL (Absolute) `Slot` (Unsigned Integer) to `POSIXTime`
slotToPOSIXTime :: Slot -> QueryM (Either SlotToPOSIXTimeError Unit)
slotToPOSIXTime slot = runExceptT do
  let ogmiosSlot = absSlotFromSlot slot
  -- Get system start and parse into `DateTime`:
  sysStart <- lift getSystemStart
  let sysStart' = unwrap sysStart
  sysStartDt <- liftedM (ParseToDataTime sysStart)
    $ liftEffect
    $ toDateTime <$> parse sysStart'
  -- Get era summaries:
  eraSummaries <- lift getEraSummaries
  currentEra <- liftEither $ findEraSummary eraSummaries ogmiosSlot
  relSlot <- liftEither $ relSlotFromAbsSlot currentEra ogmiosSlot
  liftEither $ pure unit

-- | Convert a CSL (Absolute) `Slot` (`UInt`) to an Ogmios absolute slot
-- | (`BigInt`)
absSlotFromSlot :: Slot -> AbsSlot
absSlotFromSlot = wrap <<< uIntToBigInt <<< unwrap

-- | Convert an Ogmios absolute slot (`BigInt`) to a CSL (Absolute) `Slot`
-- | (`UInt`)
slotFromAbsSlot :: AbsSlot -> Maybe Slot
slotFromAbsSlot = map wrap <<< bigIntToUInt <<< unwrap

-- | Finds the `EraSummary` an `AbsSlot` lies inside (if any).
findEraSummary
  :: EraSummariesQR
  -> AbsSlot -- Slot we are testing and trying to find inside `EraSummariesQR`
  -> Either SlotToPOSIXTimeError EraSummary
findEraSummary (EraSummariesQR eraSummaries) os@(AbsSlot ogmiosSlot) =
  note (CannotFindSlotInEraSummaries os) $ find pred eraSummaries
  where
  -- Potential FIXME: In the case of `Just`, do we want to use `safeZone` from
  -- `parameters` to provide a buffer?
  pred :: EraSummary -> Boolean
  pred (EraSummary { start, end }) =
    (unwrap start).slot <= os && maybe true ((<) os <<< _.slot <<< unwrap) end

-- getEstAbsSlot :: EraSummaryTime -> AbsSlot
-- getEstAbsSlot (EraSummaryTime es) = es.slot

-- | Relative slot of an `AbsSlot` within an `EraSummary`
newtype RelSlot = RelSlot BigInt

derive instance Generic RelSlot _
derive instance Newtype RelSlot _
derive newtype instance Eq RelSlot
derive newtype instance Ord RelSlot

instance Show RelSlot where
  show = genericShow

-- | Find the relative slot provided we know the `AbsSlot` for an absolute slot
-- | given an `EraSummary`. We could relax the `Maybe` monad if we use this
-- | in conjunction with `findEraSummary`. However, we choose to make the
-- | function more general, guarding against a larger `start`ing slot
relSlotFromAbsSlot
  :: EraSummary -> AbsSlot -> Either SlotToPOSIXTimeError RelSlot
relSlotFromAbsSlot (EraSummary { start }) os@(AbsSlot ogmiosSlot) = do
  let startSlot = unwrap (unwrap start).slot
  unless (startSlot <= ogmiosSlot) (throwError $ StartingSlotGreaterThanSlot os)
  pure $ wrap $ ogmiosSlot - startSlot

-- relTimeFromRelSlot ::