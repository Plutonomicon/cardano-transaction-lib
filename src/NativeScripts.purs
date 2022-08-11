module NativeScripts
  ( NativeScriptHash(NativeScriptHash)
  , nativeScriptHash
  , getMaximumSigners
  ) where

import Prelude

import Cardano.Types.Transaction
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  )
import Data.Array as Array
import Data.Foldable (foldr, maximumBy)
import Data.Function (on)
import Data.List (List(Cons, Nil), (:))
import Data.List as List
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, wrap)
import Data.Set (Set)
import Data.Set as Set
import Serialization.Hash (Ed25519KeyHash, ScriptHash)
import Serialization.Hash as Hashing
import Serialization.NativeScript (convertNativeScript)

newtype NativeScriptHash = NativeScriptHash ScriptHash

derive instance Newtype NativeScriptHash _
derive newtype instance Eq NativeScriptHash
derive newtype instance Ord NativeScriptHash

instance Show NativeScriptHash where
  show (NativeScriptHash sh) = "(NativeScriptHash " <> show sh <> ")"

nativeScriptHash :: NativeScript -> Maybe NativeScriptHash
nativeScriptHash ns = wrap <<< Hashing.nativeScriptHash <$> convertNativeScript
  ns

-- | `SetChoice` is an internal type representing internal state of
-- | `getMaximumSigners` algorithm. There are a number of values (key hashes)
-- | from which we must choose exactly `choice`. It's like a suspended `NOfK`
-- | choice. We track the alternatives alongside their count to adjust for
-- | possible duplicates in `allChoice`.
type SetChoice a = Array { set :: Set a, choice :: Int }

anyChoice :: forall a. Ord a => SetChoice a -> SetChoice a -> SetChoice a
anyChoice as bs = Array.nub $ as <> bs

allChoice :: forall a. Ord a => SetChoice a -> SetChoice a -> SetChoice a
allChoice as bs = Array.concat do
  a <- as
  b <- bs
  pure
    -- We want to select a.choice from a.set and b.choice from b.set,
    -- but there are equal elements that count for both if we select them.
    -- So to satisfy the requirement we must uncount the elements present in
    -- both sets.
    [ { choice: a.choice + b.choice - Set.size (Set.intersection a.set b.set)
      , set: a.set <> b.set
      }
    ]

subsets :: forall a. List a -> List (List a)
subsets Nil = List.singleton Nil
subsets (Cons x xs) = map (Cons x) (subsets xs) <> subsets xs

subsetsOfLength :: forall a. Int -> Array a -> Array (Array a)
subsetsOfLength n =
  List.fromFoldable >>> sublists n >>> List.toUnfoldable >>> map
    List.toUnfoldable

sublists :: forall a. Int -> List a -> List (List a)
sublists n xs = List.take (List.length xs - n + 1) $ sublists' n xs
  where
  sublists' _ Nil = Cons Nil Nil
  sublists' n' xs'@(Cons _ rest) = List.take n' xs' : sublists' n' rest

-- | Used for fee calculation.
-- | We try to calculate maximum number of signers from the script itself,
-- | following its logic.
-- | But we must not count `requiredSigners` as signers from native scripts
-- | twice, because that would lead to excessive fees. Hence we accept a set
-- | of already known signers to be ignored in this function.
getMaximumSigners :: Set Ed25519KeyHash -> NativeScript -> Int
getMaximumSigners alreadyCounted =
  choices >>> maximumBy (compare `on` _.choice) >>> map _.choice >>> fromMaybe 0
  where
  choices :: NativeScript -> SetChoice Ed25519KeyHash
  choices = case _ of
    ScriptPubkey kh
      | Set.member kh alreadyCounted -> [ { choice: 0, set: Set.empty } ]
      | otherwise -> [ { choice: 1, set: Set.singleton kh } ]
    ScriptAll nss -> foldr allChoice [ { choice: 0, set: Set.empty } ]
      (choices <$> nss)
    ScriptAny nss -> foldr anyChoice [ { choice: 0, set: Set.empty } ]
      (choices <$> nss)
    ScriptNOfK n nss -> choices
      (ScriptAny $ map ScriptAll (subsetsOfLength n nss))
    TimelockStart _ -> [ { choice: 0, set: Set.empty } ]
    TimelockExpiry _ -> [ { choice: 0, set: Set.empty } ]
