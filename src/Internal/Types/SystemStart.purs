module Ctl.Internal.Types.SystemStart
  ( SystemStart(SystemStart)
  , sysStartFromOgmiosTimestamp
  , sysStartFromOgmiosTimestampUnsafe
  , sysStartToOgmiosTimestamp
  , sysStartUnixTime
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Either (Either, hush)
import Data.Formatter.DateTime (Formatter, format, parseFormatString, unformat)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (length, take) as String
import JS.BigInt (BigInt)
import JS.BigInt (fromNumber) as BigInt
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
sysStartFromOgmiosTimestamp timestamp = wrap <$> (unformatMsec <|> unformatSec)
  where
  unformatMsec :: Either String DateTime
  unformatMsec = unformat
    (mkDateTimeFormatterUnsafe ogmiosDateTimeFormatStringMsec)
    (String.take (String.length ogmiosDateTimeFormatStringMsec) timestamp)

  unformatSec :: Either String DateTime
  unformatSec = unformat
    (mkDateTimeFormatterUnsafe ogmiosDateTimeFormatStringSec)
    (String.take (String.length ogmiosDateTimeFormatStringSec) timestamp)

sysStartFromOgmiosTimestampUnsafe :: String -> SystemStart
sysStartFromOgmiosTimestampUnsafe timestamp =
  unsafePartial fromJust $ hush $ sysStartFromOgmiosTimestamp timestamp

sysStartToOgmiosTimestamp :: SystemStart -> String
sysStartToOgmiosTimestamp =
  format (mkDateTimeFormatterUnsafe ogmiosDateTimeFormatStringMsecUTC)
    <<< unwrap

mkDateTimeFormatterUnsafe :: String -> Formatter
mkDateTimeFormatterUnsafe =
  unsafePartial fromJust <<< hush <<< parseFormatString

ogmiosDateTimeFormatStringSec :: String
ogmiosDateTimeFormatStringSec = "YYYY-MM-DDTHH:mm:ss"

ogmiosDateTimeFormatStringMsec :: String
ogmiosDateTimeFormatStringMsec = ogmiosDateTimeFormatStringSec <> ".SSS"

ogmiosDateTimeFormatStringMsecUTC :: String
ogmiosDateTimeFormatStringMsecUTC = ogmiosDateTimeFormatStringMsec <> "Z"

