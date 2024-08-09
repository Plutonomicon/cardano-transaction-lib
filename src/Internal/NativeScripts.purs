module Ctl.Internal.NativeScripts
  ( NativeScriptHash(NativeScriptHash)
  , nativeScriptHash
  , getMaximumSigners
  ) where

import Prelude

import Cardano.Serialization.Lib (nativeScript_hash)
import Cardano.Types (Ed25519KeyHash, ScriptHash)
import Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  )
import Cardano.Types.NativeScript as NativeScript
import Data.Array as Array
import Data.Foldable (foldr, maximumBy)
import Data.Function (on)
import Data.List (List(Cons, Nil), (:))
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, wrap)
import Data.Set (Set)
import Data.Set as Set

newtype NativeScriptHash = NativeScriptHash ScriptHash

derive instance Newtype NativeScriptHash _
derive newtype instance Eq NativeScriptHash
derive newtype instance Ord NativeScriptHash

instance Show NativeScriptHash where
  show (NativeScriptHash sh) = "(NativeScriptHash " <> show sh <> ")"

nativeScriptHash :: NativeScript -> NativeScriptHash
nativeScriptHash = wrap <<< wrap <<< nativeScript_hash <<< NativeScript.toCsl

-- | `SetChoice` is an internal type representing internal state of
-- | `getMaximumSigners` algorithm.
type SetChoice (a :: Type) = Array (Set a)

anyChoice
  :: forall (a :: Type). Ord a => SetChoice a -> SetChoice a -> SetChoice a
anyChoice as bs = Array.nub $ as <> bs

allChoice
  :: forall (a :: Type). Ord a => SetChoice a -> SetChoice a -> SetChoice a
allChoice as bs = (<>) <$> as <*> bs

subsetsOfLength :: forall (a :: Type). Int -> Array a -> Array (Array a)
subsetsOfLength n =
  List.fromFoldable >>> sublists n >>> List.toUnfoldable >>> map
    List.toUnfoldable

sublists :: forall (a :: Type). Int -> List a -> List (List a)
sublists n xs = List.take (List.length xs - n + 1) $ sublists' n xs
  where
  sublists' :: Int -> List a -> List (List a)
  sublists' _ Nil = Cons Nil Nil
  sublists' n' xs'@(Cons _ rest) = List.take n' xs' : sublists' n' rest

-- | Used for fee calculation.
-- | We try to calculate maximum number of signers from the script itself,
-- | following its logic.
-- | But we must not count `requiredSigners` and `selfSigners` as signers from
-- | native scripts twice, because that would lead to excessive fees. Hence we
-- | accept a set of already known signers to be ignored in this function.
getMaximumSigners :: Set Ed25519KeyHash -> NativeScript -> Int
getMaximumSigners alreadyCounted =
  sizes >>> maximumBy (compare `on` Set.size) >>> map Set.size >>> fromMaybe 0
  where
  sizes :: NativeScript -> SetChoice Ed25519KeyHash
  sizes = case _ of
    ScriptPubkey kh
      | Set.member kh alreadyCounted -> emptySetChoice
      | otherwise -> [ Set.singleton kh ]
    ScriptAll nss -> foldr allChoice emptySetChoice
      (sizes <$> nss)
    ScriptAny nss -> foldr anyChoice emptySetChoice
      (sizes <$> nss)
    ScriptNOfK n nss -> sizes
      (ScriptAny $ map ScriptAll (subsetsOfLength n nss))
    TimelockStart _ -> emptySetChoice
    TimelockExpiry _ -> emptySetChoice

emptySetChoice :: forall (a :: Type). SetChoice a
emptySetChoice = [ Set.empty ]
