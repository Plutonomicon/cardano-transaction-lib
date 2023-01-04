module Ctl.Internal.Types.SystemStart
  ( SystemStart(SystemStart)
  , sysStartFromOgmiosTimestamp
  , sysStartFromOgmiosTimestampUnsafe
  , sysStartToOgmiosTimestamp
  , sysStartUnixTime
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.BigInt (BigInt)
import Data.BigInt (fromNumber) as BigInt
import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Either (Either, hush)
import Data.Formatter.DateTime (Formatter, format, parseFormatString, unformat)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

newtype SystemStart = SystemStart DateTime

derive instance Generic SystemStart _
derive instance Newtype SystemStart _
derive newtype instance Eq SystemStart

instance Show SystemStart where
  show = genericShow

-- | Returns system start Unix time in milliseconds as `BigInt`.
sysStartUnixTime :: SystemStart -> Maybe BigInt
sysStartUnixTime (SystemStart dateTime) =
  BigInt.fromNumber $ unwrap $ unInstant $ fromDateTime dateTime

-- | Attempts to parse `SystemStart` from Ogmios timestamp string.
sysStartFromOgmiosTimestamp :: String -> Either String SystemStart
sysStartFromOgmiosTimestamp timestamp =
  wrap <$> -- FIXME: Unit test with Ogmios fixtures fails.
    ( unformat ogmiosDateTimeFormatterMsec timestamp
        <|> unformat ogmiosDateTimeFormatterSec timestamp
    )

sysStartFromOgmiosTimestampUnsafe :: String -> SystemStart
sysStartFromOgmiosTimestampUnsafe timestamp =
  unsafePartial fromJust $ hush $ sysStartFromOgmiosTimestamp timestamp

sysStartToOgmiosTimestamp :: SystemStart -> String
sysStartToOgmiosTimestamp = format ogmiosDateTimeFormatterMsec <<< unwrap

ogmiosDateTimeFormatterSec :: Formatter
ogmiosDateTimeFormatterSec =
  unsafePartial fromJust $ hush $ parseFormatString "YYYY-MM-DDTHH:mm:ssZ"

ogmiosDateTimeFormatterMsec :: Formatter
ogmiosDateTimeFormatterMsec =
  unsafePartial fromJust $ hush $ parseFormatString "YYYY-MM-DDTHH:mm:ss.SSSZ"

