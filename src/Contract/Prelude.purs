-- | A custom Prelude that re-exports Purescript's prelude and further expands.
module Contract.Prelude
  ( mconcat
  , module Aff
  , module Helpers
  , module Console
  , module Effect
  , module Either
  , module Enum
  , module Foldable
  , module Generic
  , module Log.Level
  , module Maybe
  , module Newtype
  , module PurescriptPrelude
  , module Traversable
  , module Tuple
  , module TupleNested
  ) where

-- Imports for extra functions:
import Prelude

import Ctl.Internal.Helpers -- Could maybe move this somewhere better:
  ( appendFirstMaybe
  , appendLastMaybe
  , filterMapM
  , filterMapWithKeyM
  , fromJustEff
  , fromRightEff
  , liftEither
  , liftM
  , liftMWith
  , (</>)
  , (<<>>)
  , (<\>)
  ) as Helpers
import Data.Either
  ( Either(Left, Right)
  , choose
  , either
  , fromLeft
  , fromLeft'
  , fromRight
  , fromRight'
  , hush
  , isLeft
  , isRight
  , note
  , note'
  ) as Either
import Data.Enum
  ( class BoundedEnum
  , class Enum
  , Cardinality(Cardinality)
  , cardinality
  , defaultCardinality
  , defaultFromEnum
  , defaultPred
  , defaultSucc
  , defaultToEnum
  , downFrom
  , downFromIncluding
  , enumFromThenTo
  , enumFromTo
  , fromEnum
  , pred
  , succ
  , toEnum
  , toEnumWithDefaults
  , upFrom
  , upFromIncluding
  ) as Enum
import Data.Foldable
  ( class Foldable
  , all
  , and
  , any
  , elem
  , find
  , findMap
  , fold
  , foldM
  , foldMap
  , foldMapDefaultL
  , foldMapDefaultR
  , foldl
  , foldlDefault
  , foldr
  , foldrDefault
  , indexl
  , indexr
  , intercalate
  , length
  , lookup
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , notElem
  , null
  , oneOf
  , oneOfMap
  , or
  , product
  , sum
  , surround
  , surroundMap
  ) as Foldable
import Data.Foldable (class Foldable, foldr)
import Data.Generic.Rep (class Generic) as Generic
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error)) as Log.Level
import Data.Maybe
  ( Maybe(Just, Nothing)
  , fromJust
  , fromMaybe
  , fromMaybe'
  , isJust
  , isNothing
  , maybe
  , maybe'
  , optional
  ) as Maybe
import Data.Newtype (class Newtype, over, unwrap, wrap) as Newtype
import Data.Show.Generic (genericShow) as Generic
-- A lot of this module is already re-exported from Data.Foldable
import Data.Traversable
  ( class Traversable
  , Accum
  , for
  , for_
  , mapAccumL
  , mapAccumR
  , scanl
  , scanr
  , sequence
  , sequenceDefault
  , sequence_
  , traverse
  , traverseDefault
  , traverse_
  ) as Traversable
import Data.Tuple (Tuple(Tuple), curry, fst, snd, swap, uncurry) as Tuple
import Data.Tuple.Nested
  ( type (/\)
  , T10
  , T11
  , T2
  , T3
  , T4
  , T5
  , T6
  , T7
  , T8
  , T9
  , Tuple1
  , Tuple10
  , Tuple2
  , Tuple3
  , Tuple4
  , Tuple5
  , Tuple6
  , Tuple7
  , Tuple8
  , Tuple9
  , curry1
  , curry10
  , curry2
  , curry3
  , curry4
  , curry5
  , curry6
  , curry7
  , curry8
  , curry9
  , get1
  , get10
  , get2
  , get3
  , get4
  , get5
  , get6
  , get7
  , get8
  , get9
  , over1
  , over10
  , over2
  , over3
  , over4
  , over5
  , over6
  , over7
  , over8
  , over9
  , tuple1
  , tuple10
  , tuple2
  , tuple3
  , tuple4
  , tuple5
  , tuple6
  , tuple7
  , tuple8
  , tuple9
  , uncurry1
  , uncurry10
  , uncurry2
  , uncurry3
  , uncurry4
  , uncurry5
  , uncurry6
  , uncurry7
  , uncurry8
  , uncurry9
  , (/\)
  ) as TupleNested
import Effect (Effect) as Effect
import Effect.Aff (Aff) as Aff
import Effect.Aff.Class (liftAff) as Aff
import Effect.Class (liftEffect) as Effect
import Effect.Class.Console (log) as Console
-- We could potentially export `Effect.Exception`, `Effect.Aff` and `Effect`
-- although there are conflicts in `Effect.Exception` and `Effect.Aff` for
-- example, unless we prioritise one over the other.

-- Warning message if re-exports aren't explicit:
import Prelude
  ( class Applicative
  , class Apply
  , class Bind
  , class BooleanAlgebra
  , class Bounded
  , class Category
  , class CommutativeRing
  , class Discard
  , class DivisionRing
  , class Eq
  , class EuclideanRing
  , class Field
  , class Functor
  , class HeytingAlgebra
  , class Monad
  , class Monoid
  , class Ord
  , class Ring
  , class Semigroup
  , class Semigroupoid
  , class Semiring
  , class Show
  , type (~>)
  , Ordering(LT, EQ, GT)
  , Unit
  , Void
  , absurd
  , add
  , ap
  , append
  , apply
  , between
  , bind
  , bottom
  , clamp
  , compare
  , comparing
  , compose
  , conj
  , const
  , degree
  , discard
  , disj
  , div
  , eq
  , flap
  , flip
  , gcd
  , identity
  , ifM
  , join
  , lcm
  , liftA1
  , liftM1
  , map
  , max
  , mempty
  , min
  , mod
  , mul
  , negate
  , not
  , notEq
  , one
  , otherwise
  , pure
  , recip
  , show
  , sub
  , top
  , unit
  , unless
  , unlessM
  , void
  , when
  , whenM
  , zero
  , (#)
  , ($)
  , ($>)
  , (&&)
  , (*)
  , (*>)
  , (+)
  , (-)
  , (/)
  , (/=)
  , (<)
  , (<#>)
  , (<$)
  , (<$>)
  , (<*)
  , (<*>)
  , (<<<)
  , (<=)
  , (<=<)
  , (<>)
  , (<@>)
  , (=<<)
  , (==)
  , (>)
  , (>=)
  , (>=>)
  , (>>=)
  , (>>>)
  , (||)
  ) as PurescriptPrelude

mconcat
  :: forall (f :: Type -> Type) (m :: Type). Foldable f => Monoid m => f m -> m
mconcat = foldr (<>) mempty
