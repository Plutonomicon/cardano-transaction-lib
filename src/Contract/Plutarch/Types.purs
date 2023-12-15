module Contract.Plutarch.Types
  ( PRational(PRational)
  , reduce
  , numerator
  , denominator
  , recip
  , (%)
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Contract.Numeric.Rational
  ( class RationalComponent
  , Rational
  , denominator
  , numerator
  , recip
  , reduce
  ) as R
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(List, Integer)
  )
import JS.BigInt (BigInt)

newtype PRational = PRational R.Rational

derive instance Newtype PRational _

instance ToData PRational where
  toData r = List [ Integer $ numerator r, Integer $ denominator r ]

instance FromData PRational where
  fromData (List [ Integer a, Integer b ]) = reduce a b
  fromData _ = Nothing

derive newtype instance Eq PRational
derive newtype instance Ord PRational
derive newtype instance EncodeAeson PRational
derive newtype instance DecodeAeson PRational
derive newtype instance Semiring PRational
derive newtype instance Ring PRational
derive newtype instance CommutativeRing PRational
derive newtype instance EuclideanRing PRational

reduce :: forall t. R.RationalComponent t => t -> t -> Maybe PRational
reduce = map (map (map wrap)) $ R.reduce

infixl 7 reduce as %

numerator :: PRational -> BigInt
numerator = unwrap >>> R.numerator

denominator :: PRational -> BigInt
denominator = unwrap >>> R.denominator

recip :: PRational -> Maybe PRational
recip = unwrap >>> R.recip >>> map wrap

