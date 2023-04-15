module Ctl.Internal.Plutus.Types.Value
  ( Coin(Coin)
  , Value
  , coinToValue
  , flattenNonAdaAssets
  , flattenValue
  , geq
  , getLovelace
  , getValue
  , gt
  , isCoinZero
  , isZero
  , leq
  , lovelaceValueOf
  , lt
  , negation
  , scale
  , singleton
  , singleton'
  , split
  , symbols
  , unionWith
  , valueOf
  , valueToCoin
  , valueToCoin'
  ) where

import Prelude hiding (eq)

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getField
  )
import Control.Apply (lift3)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Helpers (showWithParens)
import Ctl.Internal.Plutus.Types.AssocMap (Map(Map)) as Plutus
import Ctl.Internal.Plutus.Types.AssocMap
  ( keys
  , lookup
  , mapThese
  , singleton
  , union
  ) as Plutus.Map
import Ctl.Internal.Plutus.Types.CurrencySymbol
  ( CurrencySymbol
  , adaSymbol
  , mkCurrencySymbol
  )
import Ctl.Internal.QuickCheck (genPositive, unMaybeGen)
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.TokenName (TokenName, adaToken, mkTokenName)
import Ctl.Internal.Partition (class Equipartition)
import Data.Array (concatMap, filter, replicate)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left))
import Data.Foldable (all, fold)
import Data.Generic.Rep (class Generic)
import Data.Lattice (class JoinSemilattice, class MeetSemilattice)
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Newtype (class Newtype)
import Data.These (These(Both, That, This), these)
import Data.Traversable (sequence)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt)

newtype Value = Value (Plutus.Map CurrencySymbol (Plutus.Map TokenName BigInt))

derive newtype instance ToData Value
derive newtype instance FromData Value

instance DecodeAeson Value where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    (flip getField "getValue" >=> decodeAeson >>> map Value)

instance EncodeAeson Value where
  encodeAeson (Value mph) = encodeAeson $ encodeAeson
    { "getValue": encodeAeson mph }

arbitrarySingletonValue :: Gen Value
arbitrarySingletonValue = do
  currencySymbol <- arbitrary
  tokenName <- unMaybeGen $ mkTokenName <$> arbitrary
  num <- BigInt.fromInt <$> genPositive
  pure $ singleton currencySymbol tokenName num

instance Arbitrary Value where
  arbitrary = do
    tokenNum <- chooseInt 0 10
    fold <$> (sequence $ replicate tokenNum arbitrarySingletonValue)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#eq
instance Eq Value where
  eq = checkBinRel (==)

instance Show Value where
  show (Value mp) = "(PlutusValue " <> show mp <> ")"

instance Semigroup Value where
  append = unionWith add

instance Monoid Value where
  mempty = Value (Plutus.Map [])

instance JoinSemilattice Value where
  join = unionWith max

instance MeetSemilattice Value where
  meet = unionWith min

--------------------------------------------------------------------------------
-- Coin (Ada Lovelaces)
--------------------------------------------------------------------------------
newtype Coin = Coin BigInt

derive instance Generic Coin _
derive instance Newtype Coin _
derive newtype instance Eq Coin
derive newtype instance Ord Coin
derive newtype instance DecodeAeson Coin
derive newtype instance EncodeAeson Coin
derive newtype instance Equipartition Coin
derive newtype instance Semiring Coin
derive newtype instance Ring Coin
derive newtype instance CommutativeRing Coin
derive newtype instance EuclideanRing Coin

instance Show Coin where
  show (Coin c) = showWithParens "Coin" c

instance Semigroup Coin where
  append (Coin c1) (Coin c2) = Coin (c1 + c2)

instance Monoid Coin where
  mempty = Coin zero

instance JoinSemilattice Coin where
  join (Coin c1) (Coin c2) = Coin (max c1 c2)

instance MeetSemilattice Coin where
  meet (Coin c1) (Coin c2) = Coin (min c1 c2)

-- | Get the amount of lovelaces in Ada `Coin`.
getLovelace :: Coin -> BigInt
getLovelace (Coin l) = l

-- | Create a `Value` containing only the given `Coin`.
coinToValue :: Coin -> Value
coinToValue (Coin i) = lovelaceValueOf i

-- | Get the `Coin` in the given `Value`.
valueToCoin :: Value -> Coin
valueToCoin v = Coin $ valueOf v adaSymbol adaToken

-- | Get the `Coin` in the given `Value` as a `BigInt`.
valueToCoin' :: Value -> BigInt
valueToCoin' = getLovelace <<< valueToCoin

-- | Check whether an 'Ada' value is zero.
isCoinZero :: Coin -> Boolean
isCoinZero (Coin i) = i == zero

--------------------------------------------------------------------------------
-- Public
--------------------------------------------------------------------------------

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#Value
-- | Gets the underlying `Plutus.Types.AssocMap.Map`.
getValue :: Value -> Plutus.Map CurrencySymbol (Plutus.Map TokenName BigInt)
getValue (Value mp) = mp

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#singleton
-- | Makes a `Value` containing only the given quantity of the given currency.
singleton :: CurrencySymbol -> TokenName -> BigInt -> Value
singleton cs tn = Value <<< Plutus.Map.singleton cs <<< Plutus.Map.singleton tn

-- | Creates a singleton value given two byte arrays for currency symbol and
-- | token name respectively. Returns `Nothing` when trying to create a `Value`
-- | with the Ada currency symbol and a non-empty token name.
singleton' :: ByteArray -> ByteArray -> BigInt -> Maybe Value
singleton' cs tn amount
  | cs == mempty && tn /= mempty = Nothing
  | otherwise =
      lift3 singleton (mkCurrencySymbol cs) (mkTokenName tn)
        (pure amount)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#valueOf
-- | Gets the quantity of the given currency in the `Value`.
valueOf :: Value -> CurrencySymbol -> TokenName -> BigInt
valueOf (Value mp) cs tn = fromMaybe zero $
  Plutus.Map.lookup cs mp >>= Plutus.Map.lookup tn

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Ada.html#lovelaceValueOf
-- | A Value with the given amount of Lovelace (the currency unit).
lovelaceValueOf :: BigInt -> Value
lovelaceValueOf = singleton adaSymbol adaToken

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#local-6989586621679887124
-- | Returns a new `Value` with all amounts multiplied by `s`.
scale :: BigInt -> Value -> Value
scale s (Value mp) = Value (map (map (mul s)) mp)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#symbols
-- | The list of `CurrencySymbol`s of a `Value`.
symbols :: Value -> Array CurrencySymbol
symbols (Value mp) = Plutus.Map.keys mp

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#isZero
-- | Checks whether a `Value` is zero.
isZero :: Value -> Boolean
isZero = all (all ((==) zero)) <<< getValue

negation :: Value -> Value
negation (Value mp) = Value (map (map negate) mp)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#split
-- | Splits a value into its positive and non-positive parts. The first element of
-- | the tuple contains the non-positive parts of the value, the second element
-- | contains the positive parts. The convention is non-positive parts are
-- | negated to make them positive in the output.
split :: Value -> Value /\ Value
split (Value mp) =
  let
    neg /\ pos = Plutus.Map.mapThese worker mp
  in
    negation (Value neg) /\ Value pos
  where
  worker
    :: Plutus.Map TokenName BigInt
    -> These (Plutus.Map TokenName BigInt) (Plutus.Map TokenName BigInt)
  worker mp' = Both l r
    where
    l /\ r =
      Plutus.Map.mapThese (\a -> if a <= zero then This a else That a) mp'

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionWith
-- | Combines `Value` with a binary function on `BigInt`s.
unionWith :: (BigInt -> BigInt -> BigInt) -> Value -> Value -> Value
unionWith f lhs =
  Value <<< map (map (these identity identity f)) <<< unionVal lhs

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#flattenValue
-- | Converts a value to a simple list, keeping only the non-zero amounts.
flattenValue :: Value -> Array (CurrencySymbol /\ TokenName /\ BigInt)
flattenValue (Value (Plutus.Map arr)) =
  flip concatMap arr \(cs /\ (Plutus.Map tokens)) ->
    tokens <#> \(tn /\ value) ->
      cs /\ tn /\ value

-- | Converts a value to a simple list, keeping only the non-Ada assets
-- | with non-zero amounts.
flattenNonAdaAssets :: Value -> Array (CurrencySymbol /\ TokenName /\ BigInt)
flattenNonAdaAssets = filter (notEq adaSymbol <<< fst) <<< flattenValue

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#geq
-- | Checks whether one `Value` is greater than or equal to another.
geq :: Value -> Value -> Boolean
geq = checkBinRel (>=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#gt
-- | Checks whether one `Value` is strictly greater than another.
gt :: Value -> Value -> Boolean
gt l r = not (isZero l && isZero r) && checkBinRel (>) l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#leq
-- | Checks whether one `Value` is less than or equal to another.
leq :: Value -> Value -> Boolean
leq = checkBinRel (<=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#lt
-- | Checks whether one `Value` is strictly less than another.
lt :: Value -> Value -> Boolean
lt l r = not (isZero l && isZero r) && checkBinRel (<) l r

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionVal
-- Combines two 'Value' maps.
unionVal
  :: Value
  -> Value
  -> Plutus.Map CurrencySymbol (Plutus.Map TokenName (These BigInt BigInt))
unionVal (Value lhs') (Value rhs) =
  these (map This) (map That) Plutus.Map.union <$>
    Plutus.Map.union lhs' rhs

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkPred
checkPred :: (These BigInt BigInt -> Boolean) -> Value -> Value -> Boolean
checkPred f l r = all (all f) (unionVal l r)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkBinRel
-- Check whether a binary relation holds for value pairs of two `Value` maps,
-- supplying 0 where a key is only present in one of them.
checkBinRel :: (BigInt -> BigInt -> Boolean) -> Value -> Value -> Boolean
checkBinRel f l r = checkPred (these (flip f zero) (f zero) f) l r
