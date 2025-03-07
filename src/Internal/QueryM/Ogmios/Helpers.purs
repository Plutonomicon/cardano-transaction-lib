module Ctl.Internal.QueryM.Ogmios.Helpers
  ( sysStartFromOgmiosTimestamp
  , sysStartFromOgmiosTimestampUnsafe
  , sysStartToOgmiosTimestamp
  ) where

import Prelude

import Cardano.Types.SystemStart (SystemStart)
import Control.Alt ((<|>))
import Ctl.Internal.Helpers (unsafeFromJust)
import Data.DateTime (DateTime)
import Data.Either (Either, hush)
import Data.Formatter.DateTime (Formatter, format, parseFormatString, unformat)
import Data.Newtype (unwrap, wrap)
import Data.String (length, take) as String

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
  unsafeFromJust "sysStartFromOgmiosTimestampUnsafe" $ hush $
    sysStartFromOgmiosTimestamp timestamp

sysStartToOgmiosTimestamp :: SystemStart -> String
sysStartToOgmiosTimestamp =
  format (mkDateTimeFormatterUnsafe ogmiosDateTimeFormatStringMsecUTC)
    <<< unwrap

mkDateTimeFormatterUnsafe :: String -> Formatter
mkDateTimeFormatterUnsafe =
  unsafeFromJust "mkDateTimeFormatterUnsafe" <<< hush <<< parseFormatString

ogmiosDateTimeFormatStringSec :: String
ogmiosDateTimeFormatStringSec = "YYYY-MM-DDTHH:mm:ss"

ogmiosDateTimeFormatStringMsec :: String
ogmiosDateTimeFormatStringMsec = ogmiosDateTimeFormatStringSec <> ".SSS"

ogmiosDateTimeFormatStringMsecUTC :: String
ogmiosDateTimeFormatStringMsecUTC = ogmiosDateTimeFormatStringMsec <> "Z"
