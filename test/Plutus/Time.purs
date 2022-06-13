module Test.Plutus.Time
  ( suite
  ) where

import Data.BigInt as BigInt
import Data.Maybe (Maybe(Just, Nothing))
import Prelude
import Mote (group)
import QueryM.Ogmios
  ( AbsSlot(AbsSlot)
  , CurrentEpoch(CurrentEpoch)
  , Epoch(Epoch)
  , EpochLength(EpochLength)
  , EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  , EraSummaryTime(EraSummaryTime)
  , SafeZone(SafeZone)
  , SlotLength(SlotLength)
  , SystemStart(SystemStart)
  , RelativeTime(RelativeTime)
  )
import Test.Utils (toFromAesonTest)
import TestM (TestPlanM)
import Types.Interval
  ( AbsTime(AbsTime)
  , ModTime(ModTime)
  , POSIXTime(POSIXTime)
  , PosixTimeToSlotError
      ( CannotConvertAbsSlotToSlot
      , CannotFindTimeInEraSummaries
      , CannotGetBigIntFromNumber'
      , EndSlotLessThanSlotOrModNonZero
      , PosixTimeBeforeSystemStart
      , StartTimeGreaterThanTime
      )
  , RelSlot(RelSlot)
  , RelTime(RelTime)
  , SlotToPosixTimeError
      ( CannotFindSlotInEraSummaries
      , CannotGetBigIntFromNumber
      , EndTimeLessThanTime
      , StartingSlotGreaterThanSlot
      )
  , ToOnChainPosixTimeRangeError
      ( PosixTimeToSlotError'
      , SlotToPosixTimeError'
      )
  )

absSlotFixture :: AbsSlot
absSlotFixture = mkAbsSlot 34892625

absTimeFixture :: AbsTime
absTimeFixture = AbsTime $ BigInt.fromInt 321541237

posixTimeFixture :: POSIXTime
posixTimeFixture = POSIXTime $ BigInt.fromInt 12345678

modTimeFixture :: ModTime
modTimeFixture = ModTime $ BigInt.fromInt 7836

slotToPosixTimeErrFixture :: SlotToPosixTimeError
slotToPosixTimeErrFixture = CannotFindSlotInEraSummaries absSlotFixture

posixTimeToSlotErrFixture :: PosixTimeToSlotError
posixTimeToSlotErrFixture = CannotFindTimeInEraSummaries absTimeFixture

relTimeFixture :: RelTime
relTimeFixture = RelTime $ BigInt.fromInt 85723

relSlotFixture :: RelSlot
relSlotFixture = RelSlot $ BigInt.fromInt 12855

currentEpochFixture :: CurrentEpoch
currentEpochFixture = CurrentEpoch $ BigInt.fromInt 58326646

systemStartFixture :: SystemStart
systemStartFixture = SystemStart "2019-07-24T20:20:16Z"

mkRelativeTime :: Int -> RelativeTime
mkRelativeTime = RelativeTime <<< BigInt.fromInt

mkAbsSlot :: Int -> AbsSlot
mkAbsSlot = AbsSlot <<< BigInt.fromInt

mkEpoch :: Int -> Epoch
mkEpoch = Epoch <<< BigInt.fromInt

mkEpochLength :: Int -> EpochLength
mkEpochLength = EpochLength <<< BigInt.fromInt

mkSlotLength :: Int -> SlotLength
mkSlotLength = SlotLength <<< BigInt.fromInt

mkSafeZone :: Int -> SafeZone
mkSafeZone = SafeZone <<< BigInt.fromInt

eraSummariesFixture :: EraSummaries
eraSummariesFixture = EraSummaries
  [ EraSummary
      { "start": EraSummaryTime
          { "time": RelativeTime zero
          , "slot": AbsSlot zero
          , "epoch": Epoch zero
          }
      , "end": Just $ EraSummaryTime
          { "time": mkRelativeTime 31968000
          , "slot": mkAbsSlot 1598400
          , "epoch": mkEpoch 74
          }
      , "parameters": EraSummaryParameters
          { "epochLength": mkEpochLength 21600
          , "slotLength": mkSlotLength 20
          , "safeZone": mkSafeZone 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": mkRelativeTime 31968000
          , "slot": mkAbsSlot 1598400
          , "epoch": mkEpoch 74
          }
      , "end": Just $ EraSummaryTime
          { "time": mkRelativeTime 44064000
          , "slot": mkAbsSlot 13694400
          , "epoch": mkEpoch 102
          }
      , "parameters": EraSummaryParameters
          { "epochLength": mkEpochLength 432000
          , "slotLength": SlotLength one
          , "safeZone": mkSafeZone 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": mkRelativeTime 44064000
          , "slot": mkAbsSlot 13694400
          , "epoch": mkEpoch 102
          }
      , "end": Just $ EraSummaryTime
          { "time": mkRelativeTime 48384000
          , "slot": mkAbsSlot 18014400
          , "epoch": mkEpoch 112
          }
      , "parameters": EraSummaryParameters
          { "epochLength": mkEpochLength 432000
          , "slotLength": SlotLength one
          , "safeZone": mkSafeZone 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": mkRelativeTime 48384000
          , "slot": mkAbsSlot 18014400
          , "epoch": mkEpoch 112
          }
      , "end": Just $ EraSummaryTime
          { "time": mkRelativeTime 66528000
          , "slot": mkAbsSlot 36158400
          , "epoch": mkEpoch 154
          }
      , "parameters": EraSummaryParameters
          { "epochLength": mkEpochLength 432000
          , "slotLength": SlotLength one
          , "safeZone": mkSafeZone 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": mkRelativeTime 66528000
          , "slot": mkAbsSlot 36158400
          , "epoch": mkEpoch 154
          }
      , "end": Nothing
      , "parameters": EraSummaryParameters
          { "epochLength": mkEpochLength 432000
          , "slotLength": SlotLength one
          , "safeZone": mkSafeZone 129600
          }
      }
  ]

suite :: TestPlanM Unit
suite = do
  group "Time-related Aeson representation tests" do
    toFromAesonTest "POSIXTime" posixTimeFixture
    group "SlotToPosixTimeError" do
      toFromAesonTest "CannotFindSlotInEraSummaries" slotToPosixTimeErrFixture
      toFromAesonTest "StartingSlotGreaterThanSlot" $
        StartingSlotGreaterThanSlot absSlotFixture
      toFromAesonTest "EndTimeLessThanTime" $ EndTimeLessThanTime absTimeFixture
      toFromAesonTest "CannotGetBigIntFromNumber" CannotGetBigIntFromNumber
    group "PosixTimeToSlotError" do
      toFromAesonTest "CannotConvertAbsSlotToSlot" posixTimeToSlotErrFixture
      toFromAesonTest "PosixTimeBeforeSystemStart" $ PosixTimeBeforeSystemStart
        posixTimeFixture
      toFromAesonTest "StartTimeGreaterThanTime" $ StartTimeGreaterThanTime
        absTimeFixture
      toFromAesonTest "EndSlotLessThanSlotOrModNonZero" $
        EndSlotLessThanSlotOrModNonZero absSlotFixture modTimeFixture
      toFromAesonTest "CannotConvertAbsSlotToSlot" $ CannotConvertAbsSlotToSlot
        absSlotFixture
      toFromAesonTest "CannotGetBigIntFromNumber'" $ CannotGetBigIntFromNumber'
    group "ToOnChainPosixTimeRangeError" do
      toFromAesonTest "PosixTimeToSlotError'" $ PosixTimeToSlotError'
        posixTimeToSlotErrFixture
      toFromAesonTest "SlotToPosixTimeError'" $ SlotToPosixTimeError'
        slotToPosixTimeErrFixture
    group "Misc. Types" do
      toFromAesonTest "AbsSlot" absSlotFixture
      toFromAesonTest "AbsTime" absTimeFixture
      toFromAesonTest "RelSlot" relSlotFixture
      toFromAesonTest "RelTime" relTimeFixture
      toFromAesonTest "EraSummaries" eraSummariesFixture
      toFromAesonTest "SystemStart" systemStartFixture
      toFromAesonTest "CurrentEpoch" currentEpochFixture
