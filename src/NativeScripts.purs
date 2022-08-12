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
-- | `getMaximumSigners` algorithm.
type SetChoice a = Array { set :: Set a, size :: Int }

anyChoice :: forall a. Ord a => SetChoice a -> SetChoice a -> SetChoice a
anyChoice as bs = Array.nub $ as <> bs

allChoice :: forall a. Ord a => SetChoice a -> SetChoice a -> SetChoice a
allChoice as bs = Array.concat do
  a <- as
  b <- bs
  let
    set = a.set <> b.set
  pure [ { size: Set.size set, set } ]

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
  sizes >>> maximumBy (compare `on` _.size) >>> map _.size >>> fromMaybe 0
  where
  sizes :: NativeScript -> SetChoice Ed25519KeyHash
  sizes = case _ of
    ScriptPubkey kh
      | Set.member kh alreadyCounted -> emptySetChoice
      | otherwise -> [ { size: 1, set: Set.singleton kh } ]
    ScriptAll nss -> foldr allChoice emptySetChoice
      (sizes <$> nss)
    ScriptAny nss -> foldr anyChoice emptySetChoice
      (sizes <$> nss)
    ScriptNOfK n nss -> sizes
      (ScriptAny $ map ScriptAll (subsetsOfLength n nss))
    TimelockStart _ -> emptySetChoice
    TimelockExpiry _ -> emptySetChoice

emptySetChoice :: forall a. SetChoice a
emptySetChoice = [ { size: 0, set: Set.empty } ]
