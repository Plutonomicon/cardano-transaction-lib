module Test.Plutus.Time
  ( suite
  ) where

import Data.BigInt as BigInt
import Data.Maybe (Maybe(Just, Nothing))
import Prelude
import Mote (group)
import QueryM.Ogmios
  ( CurrentEpoch(CurrentEpoch)
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
import Serialization.Address (Slot(Slot))
import Types.BigNum as BigNum
import Types.Interval
  ( AbsTime(AbsTime)
  , ModTime(ModTime)
  , POSIXTime(POSIXTime)
  , PosixTimeToSlotError
      ( CannotFindTimeInEraSummaries
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

slotFixture :: Slot
slotFixture = mkSlot 34892625

absTimeFixture :: AbsTime
absTimeFixture = AbsTime $ BigInt.fromInt 321541237

posixTimeFixture :: POSIXTime
posixTimeFixture = POSIXTime $ BigInt.fromInt 12345678

modTimeFixture :: ModTime
modTimeFixture = ModTime $ BigInt.fromInt 7836

slotToPosixTimeErrFixture :: SlotToPosixTimeError
slotToPosixTimeErrFixture = CannotFindSlotInEraSummaries slotFixture

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

mkSlot :: Int -> Slot
mkSlot = Slot <<< BigNum.fromInt

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
          , "slot": Slot BigNum.zero
          , "epoch": Epoch zero
          }
      , "end": Just $ EraSummaryTime
          { "time": mkRelativeTime 31968000
          , "slot": mkSlot 1598400
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
          , "slot": mkSlot 1598400
          , "epoch": mkEpoch 74
          }
      , "end": Just $ EraSummaryTime
          { "time": mkRelativeTime 44064000
          , "slot": mkSlot 13694400
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
          , "slot": mkSlot 13694400
          , "epoch": mkEpoch 102
          }
      , "end": Just $ EraSummaryTime
          { "time": mkRelativeTime 48384000
          , "slot": mkSlot 18014400
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
          , "slot": mkSlot 18014400
          , "epoch": mkEpoch 112
          }
      , "end": Just $ EraSummaryTime
          { "time": mkRelativeTime 66528000
          , "slot": mkSlot 36158400
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
          , "slot": mkSlot 36158400
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
        StartingSlotGreaterThanSlot slotFixture
      toFromAesonTest "EndTimeLessThanTime" $ EndTimeLessThanTime absTimeFixture
      toFromAesonTest "CannotGetBigIntFromNumber" CannotGetBigIntFromNumber
    group "PosixTimeToSlotError" do
      toFromAesonTest "PosixTimeBeforeSystemStart" $ PosixTimeBeforeSystemStart
        posixTimeFixture
      toFromAesonTest "StartTimeGreaterThanTime" $ StartTimeGreaterThanTime
        absTimeFixture
      toFromAesonTest "EndSlotLessThanSlotOrModNonZero" $
        EndSlotLessThanSlotOrModNonZero slotFixture modTimeFixture
      toFromAesonTest "CannotGetBigIntFromNumber'" $ CannotGetBigIntFromNumber'
    group "ToOnChainPosixTimeRangeError" do
      toFromAesonTest "PosixTimeToSlotError'" $ PosixTimeToSlotError'
        posixTimeToSlotErrFixture
      toFromAesonTest "SlotToPosixTimeError'" $ SlotToPosixTimeError'
        slotToPosixTimeErrFixture
    group "Misc. Types" do
      toFromAesonTest "Slot" slotFixture
      toFromAesonTest "AbsTime" absTimeFixture
      toFromAesonTest "RelSlot" relSlotFixture
      toFromAesonTest "RelTime" relTimeFixture
      toFromAesonTest "EraSummaries" eraSummariesFixture
      toFromAesonTest "SystemStart" systemStartFixture
      toFromAesonTest "CurrentEpoch" currentEpochFixture
