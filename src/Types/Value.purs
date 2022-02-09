module Types.Value
  ( Coin(..)
  , CurrencySymbol
  , NonAdaAsset
  , TokenName
  , Value
  , eq
  , filterNonAda
  , valueToCoin
  , valueToCoin'
  , geq
  , getCurrencySymbol
  , getLovelace
  , getNonAdaAsset
  , getNonAdaAsset'
  , getTokenName
  , gt
  , isAdaOnly
  , isPos
  , isZero
  , leq
  , lovelaceValueOf
  , lt
  , minus
  , mkCoin
  , mkCurrencySymbol
  , mkNonAdaAsset
  , mkNonAdaAssets
  , mkNonAdaAssetsFromTokenMap
  , mkSingletonNonAdaAsset
  , mkSingletonValue
  , mkTokenName
  , mkTokenNames
  , mkValue
  , numCurrencySymbols
  , numTokenNames
  , sumTokenNameLengths
  , coinToValue
  , valueOf
  )
  where

import Prelude
import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array (filter)
import Data.BigInt (BigInt, fromInt)
import Data.Bitraversable (bitraverse, ltraverse)
import Data.Foldable (any, fold, foldl, length)
import Data.Generic.Rep (class Generic)
import Data.List ((:), all, List(Nil))
import Data.Map (keys, lookup, Map, member, toUnfoldable, unions, values)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.These (These(Both, That, This))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested ((/\), type (/\))

import Serialization.Hash (scriptHashFromBytes)
import Types.ByteArray (ByteArray, byteLength)

-- Should we newtype wrap this over Ada or remove Ada completely.
newtype Coin = Coin BigInt

derive instance Generic Coin _
derive instance Newtype Coin _
derive newtype instance eqCoin :: Eq Coin

instance Show Coin where
  show = genericShow

instance semigroupCoin :: Semigroup Coin where
  append (Coin c1) (Coin c2) = Coin (c1 + c2)

instance monoidCoin :: Monoid Coin where
  mempty = Coin zero

-- This module rewrites functionality from:
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value

mkCoin :: Int -> Coin
mkCoin = Coin <<< fromInt

getLovelace :: Coin -> BigInt
getLovelace (Coin l) = l

lovelaceValueOf :: BigInt -> Value
lovelaceValueOf = flip (Value <<< Coin) mempty

-- | Create a 'Value' containing only the given 'Coin/Ada'.
coinToValue :: Coin -> Value
coinToValue (Coin i) = lovelaceValueOf i

-- | Get the 'Coin/Ada' in the given 'Value'.
valueToCoin :: Value -> Coin
valueToCoin = Coin <<< valueToCoin'

valueToCoin' :: Value -> BigInt
valueToCoin' v = valueOf v adaSymbol adaToken

newtype CurrencySymbol = CurrencySymbol ByteArray

derive newtype instance eqCurrencySymbol :: Eq CurrencySymbol
derive newtype instance ordCurrencySymbol :: Ord CurrencySymbol

instance showCurrencySymbol :: Show CurrencySymbol where
  show (CurrencySymbol cs) = show cs

getCurrencySymbol :: CurrencySymbol -> ByteArray
getCurrencySymbol (CurrencySymbol curSymbol) = curSymbol

-- Currency symbol for Ada, do not use inside NonAdaAsset map - do not export.
-- For internal use only
adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol mempty

-- | Create a CurrencySymbol from a ByteArray since CurrencySymbol data
-- | constructor is not exported
mkCurrencySymbol :: ByteArray -> Maybe CurrencySymbol
mkCurrencySymbol byteArr =
  scriptHashFromBytes byteArr *> pure (CurrencySymbol byteArr)

-- Create an Ada CurrencySymbol from a ByteArray
mkAdaSymbol :: ByteArray -> Maybe CurrencySymbol
mkAdaSymbol byteArr =
  if byteLength byteArr == 0 then pure adaSymbol else Nothing

newtype TokenName = TokenName ByteArray

derive newtype instance eqTokenName :: Eq TokenName
derive newtype instance ordTokenName :: Ord TokenName

instance showTokenName :: Show TokenName where
  show (TokenName tn) = show tn

getTokenName :: TokenName -> ByteArray
getTokenName (TokenName tokenName) = tokenName

-- Token name for Ada - do not export. For intternal use only
adaToken :: TokenName
adaToken = TokenName mempty

-- Do we really need a smart constructor for TokenName if all we are doing
-- is checking its length? Is it safe to export its constructor?
-- | Create a TokenName from a ByteArray since TokenName data constructor is not
-- | exported
mkTokenName :: ByteArray -> Maybe TokenName
mkTokenName byteArr =
  if byteLength byteArr >= 0 then pure $ TokenName byteArr else Nothing

-- | Creates a Map of TokenName and Big Integers from a Traversable of 2-tuple
-- | ByteArray and Big Integers with the possibility of failure
mkTokenNames
  ::  forall t
   . Traversable t
  => t (ByteArray /\ BigInt)
  -> Maybe (Map TokenName BigInt)
mkTokenNames = traverse (ltraverse mkTokenName) >>> map Map.fromFoldable

-- -- Create an Ada TokenName from a ByteArray - do not export
-- unsafeMkAdaToken :: ByteArray -> Maybe TokenName
-- unsafeMkAdaToken byteArr =
--   if byteLength byteArr == 0 then pure adaToken else Nothing

newtype NonAdaAsset = NonAdaAsset (Map CurrencySymbol (Map TokenName BigInt))

derive newtype instance eqNonAdaAsset :: Eq NonAdaAsset

instance showNonAdaAsset :: Show NonAdaAsset where
  show (NonAdaAsset nonAdaAsset) = show nonAdaAsset

instance semigroupNonAdaAsset :: Semigroup NonAdaAsset where
  append = unionWith (+)

instance monoidNonAdaAsset :: Monoid NonAdaAsset where
  mempty = NonAdaAsset Map.empty

mkSingletonNonAdaAsset
  :: CurrencySymbol
  -> TokenName
  -> BigInt
  -> Maybe NonAdaAsset
mkSingletonNonAdaAsset curSymbol tokenName amount =
  if curSymbol == adaSymbol then Nothing
  else
    pure $ NonAdaAsset $ Map.singleton curSymbol $ Map.singleton tokenName amount

-- Assume all CurrencySymbol are well-formed at this point, since they come from
-- mkCurrencySymbol. If someone somehow puts an adaSymbol in, it fails.
-- Fix me: can we assume CS are safely constructed at this point?
mkNonAdaAsset :: Map CurrencySymbol (Map TokenName BigInt) -> Maybe NonAdaAsset
mkNonAdaAsset m =
  if adaSymbol `member` m then Nothing
  else pure $ NonAdaAsset m

mkNonAdaAssetsFromTokenMap'
  :: forall t
  . Traversable t
  => t (ByteArray /\ Map TokenName BigInt)
  -> Maybe (Map CurrencySymbol (Map TokenName BigInt))
mkNonAdaAssetsFromTokenMap' =
  traverse (ltraverse mkCurrencySymbol) >>> map Map.fromFoldable

-- Don't need mkNonAdaAsset here, we could just use Data Constructor since
-- mkCurrencySymbol is called inside mkNonAdaAssets'
mkNonAdaAssetsFromTokenMap
  :: forall t
  . Traversable t
  => t (ByteArray /\ Map TokenName BigInt)
  -> Maybe NonAdaAsset
mkNonAdaAssetsFromTokenMap xs = mkNonAdaAssetsFromTokenMap' xs >>= mkNonAdaAsset

mkNonAdaAssets'
  :: forall s t
  .  Traversable s
  => Traversable t
  => s (ByteArray /\ t (ByteArray /\ BigInt))
  -> Maybe (Map CurrencySymbol (Map TokenName BigInt))
mkNonAdaAssets' =
  traverse (bitraverse mkCurrencySymbol mkTokenNames) >>> map Map.fromFoldable

-- Don't need mkNonAdaAsset here, we could just use Data Constructor since
-- mkCurrencySymbol is called inside mkNonAdaAssets'
-- | Given a Traversable of ByteArrays and amounts to safely convert into a
-- | NonAdaAsset
mkNonAdaAssets
  :: forall s t
  .  Traversable s
  => Traversable t
  => s (ByteArray /\ t (ByteArray /\ BigInt))
  -> Maybe NonAdaAsset
mkNonAdaAssets xs = mkNonAdaAssets' xs >>= mkNonAdaAsset

getNonAdaAsset :: Value -> NonAdaAsset
getNonAdaAsset (Value _ nonAdaAsset) = nonAdaAsset

getNonAdaAsset' :: Value -> Map CurrencySymbol (Map TokenName BigInt)
getNonAdaAsset' (Value _ (NonAdaAsset nonAdaAsset)) = nonAdaAsset

-- | In Plutus, Ada is is stored inside the map (with currency symbol and token
-- | name being empty bytestrings). cardano-serialization-lib makes semantic
-- | distinction between native tokens and Ada, and we follow this convention.
data Value = Value Coin NonAdaAsset

derive instance eqValue :: Eq Value

instance showValue :: Show Value where
  show (Value coin nonAdaAsset) =
    show coin
      <> " and NonAdaAssets: "
      <> show nonAdaAsset

instance semigroupValue :: Semigroup Value where
  append (Value c1 m1) (Value c2 m2) = Value (c1 <> c2) (m1 <> m2)

instance monoidValue :: Monoid Value where
  mempty = Value mempty mempty

-- Do we want to do checks on NonAdaAsset just in case?
-- | Create a Value from Coin and NonAdaAsset, the latter should have been
-- | constructed safely at this point.
mkValue :: Coin -> NonAdaAsset -> Value
mkValue coin nonAdaAsset = Value coin nonAdaAsset

-- | Creates a singleton value given two byte arrays for currency symbol and
-- | token name respectively
mkSingletonValue :: ByteArray -> ByteArray -> BigInt -> Maybe Value
mkSingletonValue curSymbol' tokenName' amount = do
  curSymbol <- mkCurrencySymbol curSymbol' <|> mkAdaSymbol curSymbol'
  tokenName <- mkTokenName tokenName'
  mkSingletonValue' curSymbol tokenName amount

-- | Similar to mkSingletonValue but the user has a CurrencySymbol and TokenName at hand
mkSingletonValue' :: CurrencySymbol -> TokenName -> BigInt -> Maybe Value
mkSingletonValue' curSymbol tokenName amount =
  if curSymbol == adaSymbol then
    if tokenName == adaToken then pure $ Value (Coin amount) mempty
    else Nothing -- can't a non-empty TokenName with Ada CurrencySymbol
  else
    pure $ Value mempty $ NonAdaAsset $ Map.singleton curSymbol $ Map.singleton tokenName amount

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/src/PlutusTx.AssocMap.html#union
-- | Combine two 'Map's.
union :: ∀ k v r. Ord k => Map k v -> Map k r -> Map k (These v r)
union l r =
  let
    ls :: Array (k /\ v)
    ls = Map.toUnfoldable l

    rs :: Array (k /\ r)
    rs = Map.toUnfoldable r

    f :: v -> Maybe r -> These v r
    f a b' = case b' of
      Nothing -> This a
      Just b -> Both a b

    ls' :: Array (k /\ These v r)
    ls' = map (\(c /\ i) -> (c /\ f i (Map.lookup c (Map.fromFoldable rs)))) ls

    rs' :: Array (k /\ r)
    rs' = filter (\(c /\ _) -> not (any (\(c' /\ _) -> c' == c) ls)) rs

    rs'' :: Array (k /\ These v r)
    rs'' = map (map That) rs'
  in
    Map.fromFoldable (ls' <> rs'')

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionVal
-- | Combine two 'NonAdaAsset' maps
unionNonAda
  :: NonAdaAsset
  -> NonAdaAsset
  -> Map CurrencySymbol (Map TokenName (These BigInt BigInt))
unionNonAda (NonAdaAsset l) (NonAdaAsset r) =
  let
    combined
      :: Map CurrencySymbol (These (Map TokenName BigInt) (Map TokenName BigInt))
    combined = union l r

    unBoth
      :: These (Map TokenName BigInt) (Map TokenName BigInt)
      -> Map TokenName (These BigInt BigInt)
    unBoth k = case k of
      This a -> This <$> a
      That b -> That <$> b
      Both a b -> union a b
  in
    unBoth <$> combined

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionWith
unionWith
  :: (BigInt -> BigInt -> BigInt)
  -> NonAdaAsset
  -> NonAdaAsset
  -> NonAdaAsset
unionWith f ls rs =
  let
    combined :: Map CurrencySymbol (Map TokenName (These BigInt BigInt))
    combined = unionNonAda ls rs

    unBoth :: These BigInt BigInt -> BigInt
    unBoth k' = case k' of
      This a -> f a zero
      That b -> f zero b
      Both a b -> f a b
  in
    NonAdaAsset $ map unBoth <$> combined

-- Based on https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#flattenValue
-- | Flattens non-Ada Value into a list
flattenNonAdaValue :: NonAdaAsset -> List (CurrencySymbol /\ TokenName /\ BigInt)
flattenNonAdaValue (NonAdaAsset nonAdaAsset) = do
  cs /\ m <- toUnfoldable nonAdaAsset
  tn /\ a <- toUnfoldable m
  guard $ a /= zero
  pure $ cs /\ tn /\ a

-- | Flattens Value guarding against zeros
flattenValue :: Value -> List (CurrencySymbol /\ TokenName /\ BigInt)
flattenValue (Value coin@(Coin lovelaces) nonAdaAsset) =
  let
    flattenedNonAda :: List (CurrencySymbol /\ TokenName /\ BigInt)
    flattenedNonAda = flattenNonAdaValue nonAdaAsset
  in
    case coin == mempty of
      true -> flattenedNonAda
      false -> (adaSymbol /\ adaToken /\ lovelaces) : flattenedNonAda

-- From https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- | Converts a single tuple to Value
unflattenValue :: CurrencySymbol /\ TokenName /\ BigInt -> Maybe Value
unflattenValue (curSymbol /\ tokenName /\ amount) =
  mkSingletonValue' curSymbol tokenName amount

-- | Predicate on whether some Value contains Ada only.
isAdaOnly :: Value -> Boolean
isAdaOnly v =
  case flattenValue v of
    (cs /\ tn /\ _) : Nil ->
      cs == adaSymbol &&
        tn == adaToken
    _ -> false

-- From https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
minus :: Value -> Value -> Maybe Value
minus x y = do
  let
    negativeValues :: List (CurrencySymbol /\ TokenName /\ BigInt)
    negativeValues = flattenValue y <#>
      (\(c /\ t /\ a) -> c /\ t /\ negate a)
  y' <- traverse unflattenValue negativeValues
  pure $ x <> fold y'

-- From https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- "isValueNat" uses flattenValue which guards against zeros, so non-strict
-- inequality is redundant. So we use strict equality instead.
isPos :: Value -> Boolean
isPos = all (\(_ /\ _ /\ a) -> a > zero) <<< flattenValue

-- From https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#isZero
-- | Check whether a 'Value' is zero.
isZero :: Value -> Boolean
isZero (Value coin (NonAdaAsset nonAdaAsset)) =
  all (all ((==) zero)) nonAdaAsset && coin == mempty

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkPred
checkPred :: (These BigInt BigInt -> Boolean) -> Value -> Value -> Boolean
checkPred f (Value (Coin l) ls) (Value (Coin r) rs) =
  let
    inner :: Map TokenName (These BigInt BigInt) -> Boolean
    inner = all f -- this "all" may need to be checked?
  in
    f (Both l r) && all inner (unionNonAda ls rs) -- this "all" may need to be checked?

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkBinRel
-- | Check whether a binary relation holds for value pairs of two 'Value' maps,
-- |  supplying 0 where a key is only present in one of them.
checkBinRel :: (BigInt -> BigInt -> Boolean) -> Value -> Value -> Boolean
checkBinRel f l r =
  let
    unThese :: These BigInt BigInt -> Boolean
    unThese k' = case k' of
      This a -> f a zero
      That b -> f zero b
      Both a b -> f a b
  in
    checkPred unThese l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#geq
-- | Check whether one 'Value' is greater than or equal to another. See 'Value' for an explanation of how operations on 'Value's work.
geq :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
geq = checkBinRel (>=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#gt
-- | Check whether one 'Value' is strictly greater than another. See 'Value' for an explanation of how operations on 'Value's work.
gt :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
gt l r = not (isZero l && isZero r) && checkBinRel (>) l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#leq
-- | Check whether one 'Value' is less than or equal to another. See 'Value' for an explanation of how operations on 'Value's work.
leq :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
leq = checkBinRel (<=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#lt
-- | Check whether one 'Value' is strictly less than another. See 'Value' for an explanation of how operations on 'Value's work.
lt :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
lt l r = not (isZero l && isZero r) && checkBinRel (<) l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#eq
-- | Check whether one 'Value' is equal to another. See 'Value' for an explanation of how operations on 'Value's work.
eq :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
eq = checkBinRel (==)

isAda :: CurrencySymbol -> TokenName -> Boolean
isAda curSymbol tokenName =
  curSymbol == adaSymbol &&
    tokenName == adaToken

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#valueOf
-- | Get the quantity of the given currency in the 'Value'.
valueOf :: Value -> CurrencySymbol -> TokenName -> BigInt
valueOf (Value (Coin lovelaces) (NonAdaAsset nonAdaAsset)) curSymbol tokenName =
  case isAda curSymbol tokenName of -- FIX THIS
    false ->
      case lookup curSymbol nonAdaAsset of
        Nothing -> zero
        Just i -> case lookup tokenName i of
          Nothing -> zero
          Just v -> v
    true -> lovelaces

-- | The number of distinct currency symbols, i.e. the number of policy IDs
-- | including Ada in Coin.
numCurrencySymbols :: Value -> BigInt
numCurrencySymbols (Value coin (NonAdaAsset nonAdaAsset)) =
  case coin == mempty of
    false -> fromInt $ 1 + length nonAdaAsset
    true -> fromInt $ length nonAdaAsset -- FIX ME: Should we count this regardless whether it's zero?

-- Don't export this, we don't really care about the v in k,v.
unsafeAllTokenNames' :: Value -> Map TokenName BigInt
unsafeAllTokenNames' (Value coin@(Coin lovelaces) (NonAdaAsset nonAdaAsset)) =
  let
    nonAdaUnion :: Map TokenName BigInt
    nonAdaUnion = unions $ values nonAdaAsset
  in
    case coin == mempty of
      false -> nonAdaUnion
      true -> Map.singleton adaToken lovelaces `Map.union` nonAdaUnion

-- Don't export
unsafeAllTokenNames :: Value -> Set TokenName -- FIX: CAN'T IMPORT THIS??
unsafeAllTokenNames = keys <<< unsafeAllTokenNames'

-- | The number of distinct token names.
numTokenNames :: Value -> BigInt
numTokenNames = length <<< unsafeAllTokenNames'

-- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
-- The formula is actually based on the length of the  bytestring
--  representation - test this.
-- | Sum of the length of the strings of distinct token names.
sumTokenNameLengths :: Value -> BigInt
sumTokenNameLengths = foldl lenAdd zero <<< unsafeAllTokenNames
  where
  lenAdd :: BigInt -> TokenName -> BigInt
  lenAdd = \c a -> c + (fromInt <<< byteLength <<< getTokenName $ a)

-- From https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- | Filter a value to contain only non Ada assets
filterNonAda :: Value -> Value
filterNonAda (Value coins _) = Value coins mempty