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
absSlotFixture = AbsSlot $ BigInt.fromInt 34892625

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

eraSummariesFixture :: EraSummaries
eraSummariesFixture = EraSummaries
  [ EraSummary
      { "start": EraSummaryTime
          { "time": RelativeTime zero
          , "slot": AbsSlot zero
          , "epoch": Epoch zero
          }
      , "end": Just $ EraSummaryTime
          { "time": RelativeTime $ BigInt.fromInt 31968000
          , "slot": AbsSlot $ BigInt.fromInt 1598400
          , "epoch": Epoch $ BigInt.fromInt 74
          }
      , "parameters": EraSummaryParameters
          { "epochLength": EpochLength $ BigInt.fromInt 21600
          , "slotLength": SlotLength $ BigInt.fromInt 20
          , "safeZone": SafeZone $ BigInt.fromInt 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": RelativeTime $ BigInt.fromInt 31968000
          , "slot": AbsSlot $ BigInt.fromInt 1598400
          , "epoch": Epoch $ BigInt.fromInt 74
          }
      , "end": Just $ EraSummaryTime
          { "time": RelativeTime $ BigInt.fromInt 44064000
          , "slot": AbsSlot $ BigInt.fromInt 13694400
          , "epoch": Epoch $ BigInt.fromInt 102
          }
      , "parameters": EraSummaryParameters
          { "epochLength": EpochLength $ BigInt.fromInt 432000
          , "slotLength": SlotLength one
          , "safeZone": SafeZone $ BigInt.fromInt 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": RelativeTime $ BigInt.fromInt 44064000
          , "slot": AbsSlot $ BigInt.fromInt 13694400
          , "epoch": Epoch $ BigInt.fromInt 102
          }
      , "end": Just $ EraSummaryTime
          { "time": RelativeTime $ BigInt.fromInt 48384000
          , "slot": AbsSlot $ BigInt.fromInt 18014400
          , "epoch": Epoch $ BigInt.fromInt 112
          }
      , "parameters": EraSummaryParameters
          { "epochLength": EpochLength $ BigInt.fromInt 432000
          , "slotLength": SlotLength one
          , "safeZone": SafeZone $ BigInt.fromInt 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": RelativeTime $ BigInt.fromInt 48384000
          , "slot": AbsSlot $ BigInt.fromInt 18014400
          , "epoch": Epoch $ BigInt.fromInt 112
          }
      , "end": Just $ EraSummaryTime
          { "time": RelativeTime $ BigInt.fromInt 66528000
          , "slot": AbsSlot $ BigInt.fromInt 36158400
          , "epoch": Epoch $ BigInt.fromInt 154
          }
      , "parameters": EraSummaryParameters
          { "epochLength": EpochLength $ BigInt.fromInt 432000
          , "slotLength": SlotLength one
          , "safeZone": SafeZone $ BigInt.fromInt 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": RelativeTime $ BigInt.fromInt 66528000
          , "slot": AbsSlot $ BigInt.fromInt 36158400
          , "epoch": Epoch $ BigInt.fromInt 154
          }
      , "end": Nothing
      , "parameters": EraSummaryParameters
          { "epochLength": EpochLength $ BigInt.fromInt 432000
          , "slotLength": SlotLength one
          , "safeZone": SafeZone $ BigInt.fromInt 129600
          }
      }
  ]

suite :: TestPlanM Unit
suite = do
  group "Time-related Aeson representation tests" do
    group "POSIXTime" do
      let input = posixTimeFixture
      toFromAesonTest input
    group "SlotToPosixTimeError" do
      group "CannotFindSlotInEraSummaries" do
        let err = slotToPosixTimeErrFixture
        toFromAesonTest err
      group "StartingSlotGreaterThanSlot" do
        let err = StartingSlotGreaterThanSlot absSlotFixture
        toFromAesonTest err
      group "EndTimeLessThanTime" do
        let err = EndTimeLessThanTime absTimeFixture
        toFromAesonTest err
      group "CannotGetBigIntFromNumber" do
        let err = CannotGetBigIntFromNumber
        toFromAesonTest err
    group "PosixTimeToSlotError" do
      group "CannotConvertAbsSlotToSlot" do
        let err = posixTimeToSlotErrFixture
        toFromAesonTest err
      group "PosixTimeBeforeSystemStart" do
        let err = PosixTimeBeforeSystemStart posixTimeFixture
        toFromAesonTest err
      group "StartTimeGreaterThanTime" do
        let err = StartTimeGreaterThanTime absTimeFixture
        toFromAesonTest err
      group "EndSlotLessThanSlotOrModNonZero" do
        let err = EndSlotLessThanSlotOrModNonZero absSlotFixture modTimeFixture
        toFromAesonTest err
      group "CannotConvertAbsSlotToSlot" do
        let err = CannotConvertAbsSlotToSlot absSlotFixture
        toFromAesonTest err
      group "CannotGetBigIntFromNumber'" do
        let err = CannotGetBigIntFromNumber'
        toFromAesonTest err
    group "ToOnChainPosixTimeRangeError" do
      group "PosixTimeToSlotError'" do
        let err = PosixTimeToSlotError' posixTimeToSlotErrFixture
        toFromAesonTest err
      group "SlotToPosixTimeError'" do
        let err = SlotToPosixTimeError' slotToPosixTimeErrFixture
        toFromAesonTest err
    group "Misc. Types" do
      group "AbsSlot" do
        let input = absSlotFixture
        toFromAesonTest input
      group "AbsTime" do
        let input = absTimeFixture
        toFromAesonTest input
      group "RelSlot" do
        let input = relSlotFixture
        toFromAesonTest input
      group "RelTime" do
        let input = relTimeFixture
        toFromAesonTest input
      group "EraSummaries" do
        let input = eraSummariesFixture
        toFromAesonTest input
      group "SystemStart" do
        let input = systemStartFixture
        toFromAesonTest input
      group "CurrentEpoch" do
        let input = currentEpochFixture
        toFromAesonTest input