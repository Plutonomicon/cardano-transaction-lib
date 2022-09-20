module Equipartition
  ( class Equipartition
  , equipartition
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty
  ( fromArray
  , replicate
  , singleton
  , snoc
  , unsnoc
  ) as NEArray
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Maybe (Maybe(Just, Nothing))

class Equipartition a where
  equipartition :: a -> Int -> NonEmptyArray a

instance Equipartition BigInt where
  equipartition bi numParts
    | numParts <= one =
        NEArray.singleton bi
    | otherwise =
        mapLastN (bi `mod` BigInt.fromInt numParts) (add one) $
          NEArray.replicate numParts (bi / BigInt.fromInt numParts)
        where
        mapLastN
          :: forall (a :: Type) (n :: Type)
           . Ring n
          => Eq n
          => n
          -> (a -> a)
          -> NonEmptyArray a
          -> NonEmptyArray a
        mapLastN n f arr
          | n == zero = arr
          | otherwise =
              NEArray.unsnoc arr # \{ init, last } ->
                case NEArray.fromArray init of
                  Nothing ->
                    NEArray.singleton (f last)
                  Just init' ->
                    NEArray.snoc (mapLastN (n - one) f init') (f last)
