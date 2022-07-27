module BalanceTx.Collateral
  ( CollateralReturnError
      ( CollateralReturnError
      , CollateralReturnMinAdaValueCalcError
      )
  , addTxCollateral
  , addTxCollateralReturn
  , getMaxCollateralInputs
  , minRequiredCollateral
  , selectCollateral
  ) where

import Prelude

import BalanceTx.Helpers (fakeOutputWithNonAdaAssets)
import BalanceTx.UtxoMinAda (utxoMinAdaValue)
import Cardano.Types.Transaction
  ( Transaction
  , TransactionOutput
  , Utxos
  , _body
  , _collateral
  , _collateralReturn
  , _totalCollateral
  )
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.Value (Coin, NonAdaAsset)
import Cardano.Types.Value (getNonAdaAsset, mkValue, valueToCoin') as Value
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Reader.Class (asks)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldl, foldMap)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Lens.Setter ((?~))
import Data.List (List(Nil, Cons))
import Data.List as List
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Ord.Max (Max(Max))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toInt) as UInt
import QueryM (QueryM)
import Serialization.Address (Address)
import Types.BigNum (maxValue, toBigIntUnsafe) as BigNum
import Types.OutputDatum (OutputDatum(NoOutputDatum))
import Types.Transaction (TransactionInput)

minRequiredCollateral :: BigInt
minRequiredCollateral = BigInt.fromInt 5_000_000

addTxCollateral :: Array TransactionUnspentOutput -> Transaction -> Transaction
addTxCollateral collateral transaction =
  transaction # _body <<< _collateral ?~
    map (_.input <<< unwrap) collateral

getMaxCollateralInputs :: QueryM Int
getMaxCollateralInputs =
  asks _.pparams <#>
    fromMaybe 3 <<< map UInt.toInt <<< _.maxCollateralInputs <<< unwrap

--------------------------------------------------------------------------------
-- Collateral Return, Total Collateral
--------------------------------------------------------------------------------

data CollateralReturnError
  = CollateralReturnError String
  | CollateralReturnMinAdaValueCalcError

derive instance Generic CollateralReturnError _

instance Show CollateralReturnError where
  show = genericShow

-- | Sets `collateral return` and `total collateral` fields of the transaction.
-- | In the special case with an Ada-only collateral that is less than or equal
-- | to `minRequiredCollateral`, returns unmodified transaction (see NOTE).
-- |
-- | NOTE: Collateral cannot be less than `minRequiredCollateral` when
-- | selected using `selectCollateral` function in this module.
addTxCollateralReturn
  :: Array TransactionUnspentOutput
  -> Transaction
  -> Address
  -> QueryM (Either CollateralReturnError Transaction)
addTxCollateralReturn collateral transaction ownAddress =
  let
    collAdaValue :: BigInt
    collAdaValue = foldl adaValue' zero collateral

    collNonAdaAsset :: NonAdaAsset
    collNonAdaAsset = foldMap nonAdaAsset collateral
  in
    case collAdaValue <= minRequiredCollateral && collNonAdaAsset == mempty of
      true ->
        pure $ Right transaction
      false ->
        setTxCollateralReturn collAdaValue collNonAdaAsset
  where
  setTxCollateralReturn
    :: BigInt
    -> NonAdaAsset
    -> QueryM (Either CollateralReturnError Transaction)
  setTxCollateralReturn collAdaValue collNonAdaAsset = runExceptT do
    let
      maxBigNumAdaValue :: Coin
      maxBigNumAdaValue = wrap (BigNum.toBigIntUnsafe BigNum.maxValue)

      collReturnOutputRec =
        { address: ownAddress
        , amount: Value.mkValue maxBigNumAdaValue collNonAdaAsset
        , datum: NoOutputDatum
        , scriptRef: Nothing
        }

    -- Calculate the required min ada value for the collateral return output:
    minAdaValue <-
      ExceptT $ utxoMinAdaValue (wrap collReturnOutputRec)
        <#> note CollateralReturnMinAdaValueCalcError

    let
      -- Determine the actual ada value of the collateral return output:
      collReturnAda :: BigInt
      collReturnAda = unwrap $
        Max (collAdaValue - minRequiredCollateral) <> Max minAdaValue

      -- Build the final collateral return output:
      collReturnOutput :: TransactionOutput
      collReturnOutput = wrap $
        collReturnOutputRec
          { amount = Value.mkValue (wrap collReturnAda) collNonAdaAsset }

      totalCollateral :: BigInt
      totalCollateral = collAdaValue - collReturnAda

    except $
      case totalCollateral > zero of
        true ->
          -- Set collateral return and total collateral:
          Right $
            transaction # _body <<< _collateralReturn ?~ collReturnOutput
              # _body <<< _totalCollateral ?~ wrap totalCollateral
        false ->
          Left $ CollateralReturnError
            "Negative totalCollateral after covering min-utxo-ada requirement."

--------------------------------------------------------------------------------
-- Select Collateral
--------------------------------------------------------------------------------

collateralReturnMinAdaValue
  :: List TransactionUnspentOutput -> QueryM (Maybe BigInt)
collateralReturnMinAdaValue =
  utxoMinAdaValue <<< fakeOutputWithNonAdaAssets <<< foldMap nonAdaAsset

type ReturnOutMinAdaValue = BigInt

newtype CollateralCandidate =
  CollateralCandidate (List TransactionUnspentOutput /\ ReturnOutMinAdaValue)

derive instance Newtype CollateralCandidate _

instance Eq CollateralCandidate where
  eq = eq `on` (Tuple.snd <<< unwrap)

instance Ord CollateralCandidate where
  compare lhs rhs =
    case on compare (Tuple.snd <<< unwrap) lhs rhs of
      -- If two candidate utxo combinations correspond to return outputs with
      -- the same utxo min ada value, order them by the number of
      -- collateral inputs:
      EQ -> on compare (List.length <<< Tuple.fst <<< unwrap) lhs rhs
      ordering -> ordering

mkCollateralCandidate
  :: List TransactionUnspentOutput /\ Maybe ReturnOutMinAdaValue
  -> Maybe CollateralCandidate
mkCollateralCandidate (unspentOutputs /\ returnOutMinAdaValue) =
  CollateralCandidate <<< Tuple unspentOutputs <$> returnOutMinAdaValue

-- | Selects an utxo combination to use as collateral by generating all possible
-- | utxo combinations and then applying the following constraints:
-- |
-- |   1. `maxCollateralInputs` protocol parameter limits the maximum
-- |   cardinality of a single utxo combination.
-- |
-- |   2. Collateral inputs must have a total value of at least 5 Ada
-- |   (`minRequiredCollateral`).
-- |
-- |   3. We prefer utxo combinations that require the lowest utxo min ada
-- |   value for the corresponding collateral output, thus maintaining a
-- |   sufficient `totalCollateral`.
-- |
-- |   4. If two utxo combinations correspond to return outputs with the same
-- |   utxo min ada value, we prefer the one with fewer inputs.
-- |
selectCollateral
  :: Int -> Utxos -> QueryM (Maybe (List TransactionUnspentOutput))
selectCollateral maxCollateralInputs =
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
adaValue =
  Value.valueToCoin' <<< _.amount <<< unwrap <<< _.output <<< unwrap

adaValue' :: BigInt -> TransactionUnspentOutput -> BigInt
adaValue' init = add init <<< adaValue

nonAdaAsset :: TransactionUnspentOutput -> NonAdaAsset
nonAdaAsset =
  Value.getNonAdaAsset <<< _.amount <<< unwrap <<< _.output <<< unwrap

-- | Returns a list of all subsequences of the given list.
subsequences :: forall (a :: Type). List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) =
  let subs = subsequences xs in map (Cons x) subs <> subs

-- | Generates all possible combinations of list elements with the number of
-- | elements in each combination not exceeding `k` (no repetitions, no order).
combinations :: forall (a :: Type). Int -> List a -> List (List a)
combinations k =
  List.filter (\x -> List.length x <= k && not (List.null x))
    <<< subsequences
