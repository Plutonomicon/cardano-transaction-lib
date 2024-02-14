module Cardano.Types.Coin where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Types.BigNum (BigNum(BigNum))
import Cardano.Types.BigNum as BigNum
import Ctl.Internal.Helpers (showWithParens)
import Ctl.Internal.Partition (class Equipartition, class Partition)
import Data.Generic.Rep (class Generic)
import Data.Lattice (class JoinSemilattice, class MeetSemilattice)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Semiring as Num
import Safe.Coerce (coerce)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat)

newtype Coin = Coin BigNum

derive instance Generic Coin _
derive instance Newtype Coin _
derive newtype instance Eq Coin
derive newtype instance Ord Coin
derive newtype instance DecodeAeson Coin
derive newtype instance EncodeAeson Coin
derive newtype instance Partition Coin
derive newtype instance Equipartition Coin

instance Arbitrary Coin where
  arbitrary = Coin <<< BigNum.fromInt <$> suchThat arbitrary (_ >= Num.zero)

instance Show Coin where
  show (Coin c) = showWithParens "Coin" c

instance JoinSemilattice Coin where
  join (Coin c1) (Coin c2) = Coin (max c1 c2)

instance MeetSemilattice Coin where
  meet (Coin c1) (Coin c2) = Coin (min c1 c2)

fromInt :: Int -> Coin
fromInt = Coin <<< BigNum.fromInt

zero :: Coin
zero = Coin BigNum.zero

add :: Coin -> Coin -> Maybe Coin
add = coerce BigNum.add
