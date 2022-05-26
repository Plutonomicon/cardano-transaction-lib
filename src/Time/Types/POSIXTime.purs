module Time.Types.POSIXTime
  ( OnchainPOSIXTimeRange(..)
  , POSIXTime(..)
  , POSIXTimeRange
  ) where

import Prelude
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Lattice
  ( class BoundedJoinSemilattice
  , class BoundedMeetSemilattice
  , class JoinSemilattice
  , class MeetSemilattice
  )
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import FromData (class FromData)
import Time.Types.Interval (Interval)
import ToData (class ToData)

--------------------------------------------------------------------------------
-- POSIXTIME Type and related
--------------------------------------------------------------------------------
-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Time.html#t:POSIXTimeRange
-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26
newtype POSIXTime = POSIXTime BigInt

derive instance Generic POSIXTime _
derive instance Newtype POSIXTime _
derive newtype instance Eq POSIXTime
derive newtype instance Ord POSIXTime
-- There isn't an Enum instance for BigInt so we derive Semiring instead which
-- has consequences on how isEmpty and overlaps are defined in
-- Types.POSIXTimeRange (Interval API).
derive newtype instance Semiring POSIXTime
derive newtype instance FromData POSIXTime
derive newtype instance ToData POSIXTime

instance Show POSIXTime where
  show = genericShow

-- | An `Interval` of `POSIXTime`s.
type POSIXTimeRange = Interval POSIXTime

-- | A newtype wrapper over `POSIXTimeRange` to represent the on-chain version
-- | of an off-chain `POSIXTimeRange`. In particular, there are a few steps
-- | in conversion:
-- | 1) `POSIXTimeRange` -> `SlotRange`
-- | 2) `SlotRange` -> `TransactionValidity`
-- | 3) `TransactionValidity` -> `OnchainPOSIXTimeRange`
-- | `OnchainPOSIXTimeRange` is intended to equal the validity range found in
-- | the on-chain `ScriptContext`
newtype OnchainPOSIXTimeRange = OnchainPOSIXTimeRange POSIXTimeRange

derive instance Generic OnchainPOSIXTimeRange _
derive instance Newtype OnchainPOSIXTimeRange _
derive newtype instance Eq OnchainPOSIXTimeRange
derive newtype instance JoinSemilattice OnchainPOSIXTimeRange
derive newtype instance BoundedJoinSemilattice OnchainPOSIXTimeRange
derive newtype instance MeetSemilattice OnchainPOSIXTimeRange
derive newtype instance BoundedMeetSemilattice OnchainPOSIXTimeRange
derive newtype instance FromData OnchainPOSIXTimeRange
derive newtype instance ToData OnchainPOSIXTimeRange
