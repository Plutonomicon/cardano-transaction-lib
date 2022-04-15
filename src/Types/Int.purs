module Types.Int
  ( Int
  , newPositive
  , newNegative
  ) where

import Prim as Prim
import Prelude as Prelude
import Serialization.Types (BigNum)
import Data.Function (on)

foreign import data Int :: Prim.Type

foreign import newPositive :: BigNum -> Int
foreign import newNegative :: BigNum -> Int
foreign import _intToStr :: Int -> Prim.String

instance Prelude.Eq Int where
  eq = Prelude.eq `on` _intToStr

instance Prelude.Show Int where
  show = _intToStr
