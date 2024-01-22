module Cardano.Types.BigInt
       ( module X
       , fromCsl
       , toCsl
       , divCeil
       )
where

import Prelude

import Cardano.Serialization.Lib (bigInt_divCeil, bigInt_fromStr, bigInt_toStr)
import Cardano.Serialization.Lib as Csl
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import JS.BigInt (BigInt, Parity(Even, Odd), Radix, and, asIntN, asUintN, binary, decimal, even, fromInt, fromNumber, fromString, fromStringAs, fromTLInt, hexadecimal, not, octal, odd, or, parity, pow, shl, shr, toInt, toNumber, toString, toStringAs, xor) as X
import Partial.Unsafe (unsafePartial)

toCsl :: BigInt -> Csl.BigInt
toCsl bi = unsafePartial $ fromJust $ toMaybe $ bigInt_fromStr $
  BigInt.toString bi

fromCsl :: Csl.BigInt -> BigInt
fromCsl bi = unsafePartial $ fromJust $ BigInt.fromString $ bigInt_toStr bi

divCeil :: BigInt -> BigInt -> BigInt
divCeil a b = fromCsl $ bigInt_divCeil (toCsl a) (toCsl b)
