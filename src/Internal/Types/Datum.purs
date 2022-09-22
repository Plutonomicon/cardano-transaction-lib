module Ctl.Internal.Types.Datum
  ( Datum(Datum)
  , unitDatum
  , module X
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson')
import Aeson.Decode as Decode
import Aeson.Encode ((>$<))
import Aeson.Encode as Encode
import Control.Lazy (defer)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.PlutusData (PlutusData)
import Ctl.Internal.Types.Transaction (DataHash(DataHash)) as X
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)

-- | Define data types mirroring Plutus `Datum`, like `Datum` itself and
-- | `Redeemer` where the latter is not to be confused with the CSL-stype
-- | `Types.Transaction.Redeemer`.

-- | `Datum` is defined as a newtype of `PlutusData`
newtype Datum = Datum PlutusData

derive instance Newtype Datum _
derive instance Generic Datum _
derive newtype instance Eq Datum
derive newtype instance FromData Datum
derive newtype instance Ord Datum
derive newtype instance ToData Datum

instance EncodeAeson Datum where
  encodeAeson' = encodeAeson' <<<
    defer (const $ Encode.encode $ unwrap >$< Encode.value)

instance DecodeAeson Datum where
  decodeAeson = defer $ const $ Decode.decode $ Datum <$> Decode.value

instance Show Datum where
  show = genericShow

unitDatum :: Datum
unitDatum = Datum $ toData unit
