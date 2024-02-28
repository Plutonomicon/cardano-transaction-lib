module Test.Ctl.Internal.Plutus.Time
  ( suite
  ) where

import Prelude

import Ctl.Internal.QueryM.Ogmios
  ( OgmiosEraSummaries(OgmiosEraSummaries)
  , OgmiosSystemStart
  )
import Ctl.Internal.Serialization.Address (Slot(Slot))
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.Epoch (Epoch(Epoch))
import Ctl.Internal.Types.EraSummaries
  ( EpochLength(EpochLength)
  , EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  , EraSummaryTime(EraSummaryTime)
  , Milliseconds
  , SafeZone(SafeZone)
  , Seconds
  , SlotLength
  )
import Ctl.Internal.Types.Interval
  ( AbsTime(AbsTime)
  , ModTime(ModTime)
  , POSIXTime(POSIXTime)
  , PosixTimeToSlotError
      ( CannotFindTimeInEraSummaries
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
  , ToOnChainPosixTimeRangeError(PosixTimeToSlotError', SlotToPosixTimeError')
  )
import Ctl.Internal.Types.SystemStart (sysStartFromOgmiosTimestampUnsafe)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff)
import JS.BigInt as BigInt
import Mote (group)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Utils (toFromAesonTest, toFromAesonTestWith)

slotFixture :: Slot
slotFixture = mkSlot 34892625

absTimeFixture :: AbsTime
absTimeFixture = AbsTime $ BigInt.fromInt 321541237

absTimeNumberFixture :: Number
absTimeNumberFixture = BigInt.toNumber $ BigInt.fromInt 321541237

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

currentEpochFixture :: Epoch
currentEpochFixture = Epoch $ BigInt.fromInt 58326646

systemStartFixture :: OgmiosSystemStart
systemStartFixture =
  wrap $ sysStartFromOgmiosTimestampUnsafe "2019-07-24T20:20:16Z"

mkSlot :: Int -> Slot
mkSlot = Slot <<< BigNum.fromInt

mkEpoch :: Int -> Epoch
mkEpoch = Epoch <<< BigInt.fromInt

mkEpochLength :: Int -> EpochLength
mkEpochLength = EpochLength <<< BigInt.fromInt

mkSlotLength :: Int -> SlotLength
mkSlotLength = wrap <<< BigInt.fromInt

mkSafeZone :: Int -> SafeZone
mkSafeZone = SafeZone <<< BigInt.fromInt

mkSeconds :: Int -> Seconds
mkSeconds = wrap <<< BigInt.fromInt

mkSeconds' :: String -> Seconds
mkSeconds' s = wrap $ unsafePartial $ fromJust $ BigInt.fromString s

mkMilliseconds :: Int -> Milliseconds
mkMilliseconds = wrap <<< BigInt.fromInt

eraSummariesFixture :: EraSummaries
eraSummariesFixture = EraSummaries
  [ EraSummary
      { "start": EraSummaryTime
          { "time": mkSeconds zero
          , "slot": Slot BigNum.zero
          , "epoch": Epoch zero
          }
      , "end": Just $ EraSummaryTime
          { "time": mkSeconds 31968000
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
          { "time": mkSeconds 31968000
          , "slot": mkSlot 1598400
          , "epoch": mkEpoch 74
          }
      , "end": Just $ EraSummaryTime
          { "time": mkSeconds 44064000
          , "slot": mkSlot 13694400
          , "epoch": mkEpoch 102
          }
      , "parameters": EraSummaryParameters
          { "epochLength": mkEpochLength 432000
          , "slotLength": mkMilliseconds one
          , "safeZone": mkSafeZone 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": mkSeconds 44064000
          , "slot": mkSlot 13694400
          , "epoch": mkEpoch 102
          }
      , "end": Just $ EraSummaryTime
          { "time": mkSeconds 48384000
          , "slot": mkSlot 18014400
          , "epoch": mkEpoch 112
          }
      , "parameters": EraSummaryParameters
          { "epochLength": mkEpochLength 432000
          , "slotLength": mkMilliseconds one
          , "safeZone": mkSafeZone 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": mkSeconds 48384000
          , "slot": mkSlot 18014400
          , "epoch": mkEpoch 112
          }
      , "end": Just $ EraSummaryTime
          { "time": mkSeconds 66528000
          , "slot": mkSlot 36158400
          , "epoch": mkEpoch 154
          }
      , "parameters": EraSummaryParameters
          { "epochLength": mkEpochLength 432000
          , "slotLength": mkMilliseconds one
          , "safeZone": mkSafeZone 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": mkSeconds 66528000
          , "slot": mkSlot 36158400
          , "epoch": mkEpoch 154
          }
      , "end": Nothing
      , "parameters": EraSummaryParameters
          { "epochLength": mkEpochLength 432000
          , "slotLength": mkMilliseconds one
          , "safeZone": mkSafeZone 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": mkSeconds' "66528000023234"
          , "slot": mkSlot 36158400
          , "epoch": mkEpoch 154
          }
      , "end": Nothing
      , "parameters": EraSummaryParameters
          { "epochLength": mkEpochLength 432000
          , "slotLength": mkMilliseconds one
          , "safeZone": mkSafeZone 129600
          }
      }
  , EraSummary
      { "start": EraSummaryTime
          { "time": mkSeconds 66528000
          , "slot": mkSlot 36158400
          , "epoch": mkEpoch 154
          }
      , "end": Nothing
      , "parameters": EraSummaryParameters
          { "epochLength": mkEpochLength 432000
          , "slotLength": mkSlotLength 1
          , "safeZone": mkSafeZone 129600
          }
      }
  ]

eraSummaryLengthToSeconds :: EraSummary -> EraSummary
eraSummaryLengthToSeconds old@(EraSummary { parameters }) =
  let
    newSlotLength :: SlotLength
    newSlotLength = wrap $ unwrap (unwrap parameters).slotLength `mod`
      BigInt.fromInt 1000

    newParameters :: EraSummaryParameters
    newParameters = wrap $ (unwrap parameters) { slotLength = newSlotLength }
  in
    wrap (unwrap old) { parameters = newParameters }

eraSummariesLengthToSeconds :: OgmiosEraSummaries -> OgmiosEraSummaries
eraSummariesLengthToSeconds (OgmiosEraSummaries values) =
  wrap $ wrap (eraSummaryLengthToSeconds <$> unwrap values)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Time-related Aeson representation tests" do
    toFromAesonTest "POSIXTime" posixTimeFixture
    group "SlotToPosixTimeError" do
      toFromAesonTest "CannotFindSlotInEraSummaries" slotToPosixTimeErrFixture
      toFromAesonTest "StartingSlotGreaterThanSlot" $
        StartingSlotGreaterThanSlot slotFixture
      toFromAesonTest "EndTimeLessThanTime" $ EndTimeLessThanTime
        absTimeNumberFixture
      toFromAesonTest "CannotGetBigIntFromNumber" CannotGetBigIntFromNumber
    group "PosixTimeToSlotError" do
      toFromAesonTest "PosixTimeBeforeSystemStart" $ PosixTimeBeforeSystemStart
        posixTimeFixture
      toFromAesonTest "StartTimeGreaterThanTime" $ StartTimeGreaterThanTime
        absTimeFixture
      toFromAesonTest "EndSlotLessThanSlotOrModNonZero" $
        EndSlotLessThanSlotOrModNonZero slotFixture modTimeFixture
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
      toFromAesonTestWith "EraSummaries" eraSummariesLengthToSeconds $
        OgmiosEraSummaries eraSummariesFixture
      toFromAesonTest "SystemStart" systemStartFixture
      toFromAesonTest "CurrentEpoch" currentEpochFixture
