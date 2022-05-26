module Types.Datum
  ( Datum(..)
  , unitDatum
  , module X
  ) where

import Prelude

import Aeson (Aeson, aesonNull, class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Aeson.Decode ((</$\>), (</*\>), (</\>), decode, null)
import Aeson.Encode ((>$<), (>/\<), encode, null)
import Control.Lazy (defer)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import FromData (class FromData)
import ToData (class ToData, toData)
import Types.PlutusData (PlutusData)
import Types.Transaction (DataHash(DataHash)) as X
import Aeson.Decode as D
import Aeson.Encode as E
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
  encodeAeson' x = pure $ (defer \_ ->  E.encode $ unwrap >$< E.value ) x

instance DecodeAeson Datum where
  decodeAeson = defer \_ -> D.decode $ (Datum <$> D.value)

instance Show Datum where
  show = genericShow

unitDatum :: Datum
unitDatum = Datum (toData unit)
