module Ctl.Internal.Types.Rational
  ( Rational
  , class RationalComponent
  , reduce
  , (%)
  , recip
  , numerator
  , denominator
  , toUnitInterval
  , fromNumber
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(UnexpectedValue)
  , caseAesonObject
  , encodeAeson
  , toStringifiedNumbersJson
  , (.:)
  )
import Cardano.FromData (class FromData)
import Cardano.ToData (class ToData)
import Cardano.Types (PlutusData(Constr, Integer), UnitInterval)
import Cardano.Types.BigNum as BigNum
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (wrap)
import Data.Ratio (Ratio)
import Data.Ratio (denominator, numerator, (%)) as Ratio
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt

-- | `Rational` is a newtype over `Ratio` with a smart constructor `reduce`
-- | that allows to create a `Rational` safely. The constructor is not exposed.
-- | `Rational` also enforces the denominator to always be positive.
newtype Rational = Rational (Ratio BigInt)

derive newtype instance Show Rational
derive newtype instance Eq Rational
derive newtype instance Ord Rational
derive newtype instance Semiring Rational
derive newtype instance Ring Rational
derive newtype instance CommutativeRing Rational

type RationalRep a =
  { numerator :: a
  , denominator :: a
  }

instance EncodeAeson Rational where
  encodeAeson r = encodeAeson
    ( { "numerator": numerator r
      , "denominator": denominator r
      }
    )

instance DecodeAeson Rational where
  decodeAeson aes = caseAesonObject
    (Left <<< UnexpectedValue <<< toStringifiedNumbersJson $ aes)
    ( \obj -> do
        (n :: BigInt) <- obj .: "numerator"
        d <- obj .: "denominator"
        maybe (Left <<< UnexpectedValue <<< toStringifiedNumbersJson $ aes) pure
          $ n % d
    )
    aes

instance EuclideanRing Rational where
  degree _ = one
  mod _ _ = zero
  div a b
    | numerator b == zero = zero
    | otherwise = Rational $
        (numerator a * denominator b) Ratio.% (denominator a * numerator b)

-- | Gives the reciprocal of a `Rational`.
-- | Returns `Nothing` if applied to `zero` since the reciprocal of zero
-- | is mathematically undefined.
recip :: Rational -> Maybe Rational
recip r
  | numerator r == zero = Nothing
  | otherwise = reduce (denominator r) (numerator r)

-- | Get the numerator of a `Rational` as `BigInt`.
numerator :: Rational -> BigInt
numerator (Rational r) = Ratio.numerator r

-- | Get the denominator of a `Rational` as `BigInt`.
denominator :: Rational -> BigInt
denominator (Rational r) = Ratio.denominator r

--------------------------------------------------------------------------------
-- FromData / ToData
--------------------------------------------------------------------------------

instance ToData Rational where
  toData r =
    Constr BigNum.zero [ Integer (numerator r), Integer (denominator r) ]

instance FromData Rational where
  fromData (Constr c [ Integer n, Integer d ])
    | c == BigNum.zero = reduce n d
  fromData _ = Nothing

--------------------------------------------------------------------------------
-- RationalComponent
--------------------------------------------------------------------------------

class RationalComponent (t :: Type) where
  -- | Our `reduce` mirrors `PlutusTx.Ratio.ratio`, not `PlutusTx.Ratio.reduce`.
  -- | Plutus' `reduce` does not do any positive denominator normalisation
  -- |
  -- | Also see:
  -- | https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/src/PlutusTx.Ratio.html#ratio
  -- | https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/src/PlutusTx.Ratio.html#reduce
  reduce :: t -> t -> Maybe Rational

infixl 7 reduce as %

instance RationalComponent BigInt where
  reduce n d
    | d == zero = Nothing
    | otherwise = Just $ Rational (n Ratio.% d)

instance RationalComponent Int where
  reduce n d = reduce (BigInt.fromInt n) (BigInt.fromInt d)

toUnitInterval :: Rational -> Maybe UnitInterval
toUnitInterval (Rational r) = do
  numerator' <- BigNum.fromBigInt $ Ratio.numerator r
  denominator' <- BigNum.fromBigInt $ Ratio.denominator r
  pure $ wrap { numerator: numerator', denominator: denominator' }

foreign import decimalToFraction
  :: Number -> { numerator :: Int, denominator :: Int }

fromNumber :: Number -> Maybe Rational
fromNumber n =
  let
    { numerator, denominator } = decimalToFraction n
  in
    reduce (BigInt.fromInt numerator) (BigInt.fromInt denominator)
