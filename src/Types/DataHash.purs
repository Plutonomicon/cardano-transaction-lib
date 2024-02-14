module Cardano.Types.DataHash where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (class AsCbor)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusData (PlutusData(Constr))
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.Helpers (compareViaCslBytes, eqOrd, showFromCbor)
import Ctl.Internal.ToData (class ToData, toData)
import Data.ByteArray (byteArrayFromIntArrayUnsafe)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, coarbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)

newtype DataHash = DataHash Csl.DataHash

derive instance Generic DataHash _
derive instance Newtype DataHash _

instance Eq DataHash where
  eq = eqOrd

instance AsCbor DataHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive newtype instance EncodeAeson DataHash
derive newtype instance DecodeAeson DataHash

-- This is not newtyped derived because it will be used for ordering a
-- `TransactionInput`, we want lexicographical ordering on the hexstring.
instance Ord DataHash where
  compare = compareViaCslBytes `on` unwrap

instance Show DataHash where
  show = unwrap >>> showFromCbor "DataHash"

-- Plutus actually has this as a zero indexed record
instance FromData DataHash where
  fromData (Constr n [ bytes ]) | n == BigNum.zero = DataHash <$>
    (fromBytes =<< fromData bytes)
  fromData _ = Nothing

-- Plutus actually has this as a zero indexed record
instance ToData DataHash where
  toData (DataHash th) = Constr BigNum.zero [ toData $ toBytes th ]

instance Arbitrary DataHash where
  arbitrary = unsafePartial $
    wrap <<< fromJust <<< fromBytes <<< byteArrayFromIntArrayUnsafe <$> vectorOf
      32
      (chooseInt 0 255)

instance Coarbitrary DataHash where
  coarbitrary (DataHash th) generator = coarbitrary (toBytes th)
    generator
