module BalanceTx.Collateral where

import Prelude

import Cardano.Types.Transaction (TransactionOutput, Utxo)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.Value (valueToCoin')
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Foldable (foldl)
import Data.Function (on)
import Data.List (List(Nil, Cons))
import Data.List as List
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import QueryM (QueryM)
import Types.Transaction (TransactionInput)

utxoMinAdaValue :: TransactionOutput -> QueryM (Maybe BigInt)
utxoMinAdaValue _ = pure Nothing -- TODO: merge `develop`

collateralReturnMinAdaValue
  :: List TransactionUnspentOutput -> QueryM (Maybe BigInt)
collateralReturnMinAdaValue _ = pure Nothing -- TODO:

newtype CollateralCandidate =
  CollateralCandidate (List TransactionUnspentOutput /\ BigInt)

derive instance Newtype CollateralCandidate _

instance Eq CollateralCandidate where
  eq = eq `on` (Tuple.snd <<< unwrap)

instance Ord CollateralCandidate where
  compare = compare `on` (Tuple.snd <<< unwrap)

mkCollateralCandidate
  :: List TransactionUnspentOutput /\ Maybe BigInt -> Maybe CollateralCandidate
mkCollateralCandidate (_ /\ Nothing) = Nothing
mkCollateralCandidate (unspentOutputs /\ Just val) =
  Just (CollateralCandidate $ unspentOutputs /\ val)

selectCollateral
  :: Int -> Utxo -> QueryM (Maybe (List TransactionUnspentOutput))
selectCollateral maxCollateralInputs =
  let
    minRequiredCollateral :: BigInt
    minRequiredCollateral = BigInt.fromInt 5_000_000

    adaValue' :: BigInt -> TransactionUnspentOutput -> BigInt
    adaValue' init = add init <<< adaValue
  in
    -- Sort candidate utxo combinations in ascending order by utxo min ada value
    -- of return output, then select the first utxo combination:
    map (map (Tuple.fst <<< unwrap) <<< List.head <<< List.sort)
      -- For each candidate utxo combination calculate
      -- the min Ada value of the corresponding collateral return output:
      <<< map (List.mapMaybe mkCollateralCandidate)
      <<< traverse (\x -> Tuple x <$> collateralReturnMinAdaValue x)
      -- Filter out all utxo combinations
      -- with total Ada value < `minRequiredCollateral`:
      <<< List.filter (\x -> foldl adaValue' zero x >= minRequiredCollateral)
      -- Get all possible non-empty utxo combinations
      -- with the number of utxos <= `maxCollateralInputs`:
      <<< combinations maxCollateralInputs
      <<< map asTxUnspentOutput
      <<< Map.toUnfoldable

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

asTxUnspentOutput
  :: TransactionInput /\ TransactionOutput -> TransactionUnspentOutput
asTxUnspentOutput (input /\ output) = wrap { input, output }

adaValue :: TransactionUnspentOutput -> BigInt
adaValue = valueToCoin' <<< _.amount <<< unwrap <<< _.output <<< unwrap

subsequences :: forall (a :: Type). List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) =
  map (Cons x) (subsequences xs) <> subsequences xs

combinations :: forall (a :: Type). Int -> List a -> List (List a)
combinations k =
  List.filter (\x -> List.length x <= k && not (List.null x))
    <<< subsequences
