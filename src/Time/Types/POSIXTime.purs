module Time.Types.POSIXTime
  ( POSIXTime(..)
  , POSIXTimeRange
  ) where

import Prelude
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Time.Types.Interval (Interval)

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

instance Show POSIXTime where
  show = genericShow

-- | An `Interval` of `POSIXTime`s.
type POSIXTimeRange = Interval POSIXTime
