module Ctl.Internal.Cardano.Types.Value
  ( AssetClass(AssetClass)
  , Coin(Coin)
  , CurrencySymbol
  , NonAdaAsset
  , Value(Value)
  , class Negate
  , class Split
  , assetToValue
  , coinToValue
  , currencyMPSHash
  , eq
  , equipartitionValueWithTokenQuantityUpperBound
  , filterNonAda
  , flattenNonAdaValue
  , geq
  , getAssetQuantity
  , getCurrencySymbol
  , getLovelace
  , getNonAdaAsset
  , getNonAdaAsset'
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
  , mkSingletonValue'
  , mkValue
  , mpsSymbol
  , negation
  , numCurrencySymbols
  , numNonAdaAssets
  , numNonAdaCurrencySymbols
  , numTokenNames
  , posNonAdaAsset
  , scriptHashAsCurrencySymbol
  , split
  , sumTokenNameLengths
  , unionWith
  , unionWithNonAda
  , unwrapNonAdaAsset
  , valueAssetClasses
  , valueAssets
  , valueOf
  , valueToCoin
  , valueToCoin'
  ) where

import Prelude hiding (join)

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , encodeAeson
  , getField
  )
import Control.Alt ((<|>))
import Control.Alternative (guard)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Helpers (encodeMap, showWithParens)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata)
import Ctl.Internal.Partition (class Equipartition, equipartition)
import Ctl.Internal.Serialization.Hash
  ( ScriptHash
  , scriptHashFromBytes
  , scriptHashToBytes
  )
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.ByteArray
  ( ByteArray
  , byteArrayFromIntArrayUnsafe
  , byteArrayToHex
  , byteLength
  , hexToByteArray
  )
import Ctl.Internal.Types.Scripts (MintingPolicyHash(MintingPolicyHash))
import Ctl.Internal.Types.TokenName
  ( TokenName
  , adaToken
  , getTokenName
  , mkTokenName
  , mkTokenNames
  )
import Data.Array (cons, filter)
import Data.Array (fromFoldable) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (replicate, singleton, zipWith) as NEArray
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt, fromInt, toNumber)
import Data.Bitraversable (bitraverse, ltraverse)
import Data.Either (Either(Left), note)
import Data.Foldable (any, fold, foldl, length)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Int (ceil) as Int
import Data.Lattice (class JoinSemilattice, class MeetSemilattice, join, meet)
import Data.List (List(Nil), all, (:))
import Data.List (nubByEq) as List
import Data.Map (Map, keys, lookup, toUnfoldable, unions, values)
import Data.Map as Map
import Data.Map.Gen (genMap)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.These (These(Both, That, This))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, suchThat, vectorOf)

-- `Negate` and `Split` seem a bit too contrived, and their purpose is to
-- combine similar behaviour without satisfying any useful laws. I wonder
-- if we're better off simply writing functions for Coin, NonAdaAsset and Value
-- to `split` and `negate` (it's just 6 functions) in total without the need of
-- a somewhat meaningless typeclass.

-- We could write a Ring instance to get `negate` but I'm not sure this would
-- make much sense for Value. Plutus uses a custom AdditiveGroup.
-- We could define a Data.Group although zero-valued tokens don't degenerate
-- from our map currently - I don't think we'd want this behaviour.
-- | Negation to create an AdditiveGroup for Value. Call it negation to not confuse
-- | with negate.
class Negate (a :: Type) where
  negation :: a -> a

-- | Split a value into its positive and non-positive parts. The first element of
-- | the tuple contains the non-positive parts of the value, the second element
-- | contains the positive parts. The convention is non-positive parts are
-- | negated to make them positive in the output.
class Split (a :: Type) where
  split :: a -> a /\ a

--------------------------------------------------------------------------------
-- Coin (Ada)
--------------------------------------------------------------------------------

newtype Coin = Coin BigInt

derive instance Generic Coin _
derive instance Newtype Coin _
derive newtype instance Eq Coin
derive newtype instance Ord Coin
derive newtype instance DecodeAeson Coin
derive newtype instance EncodeAeson Coin
derive newtype instance Equipartition Coin

instance Arbitrary Coin where
  arbitrary = Coin <<< fromInt <$> suchThat arbitrary (_ >= zero)

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

instance Negate Coin where
  negation = wrap <<< negate <<< unwrap

instance Split Coin where
  split (Coin c) =
    if c <= zero then Coin (negate c) /\ Coin zero else Coin zero /\ Coin c

-- This module rewrites functionality from:
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value

-- | Make `Coin` from an `Int` instead of `BigInt`
mkCoin :: Int -> Coin
mkCoin = Coin <<< fromInt

-- | Get the amount of lovelaces in Ada `Coin`.
getLovelace :: Coin -> BigInt
getLovelace (Coin l) = l

-- | Convert a `BigInt` to Ada-only `Value`
lovelaceValueOf :: BigInt -> Value
lovelaceValueOf = flip (Value <<< Coin) mempty

-- | Create a `Value` containing only the given `Coin`.
coinToValue :: Coin -> Value
coinToValue (Coin i) = lovelaceValueOf i

-- | Get the `Coin` in the given `Value`.
valueToCoin :: Value -> Coin
valueToCoin = Coin <<< valueToCoin'

-- | Get the `Coin` in the given `Value` as a `BigInt`
valueToCoin' :: Value -> BigInt
valueToCoin' v = valueOf v unsafeAdaSymbol adaToken

--------------------------------------------------------------------------------
-- CurrencySymbol
--------------------------------------------------------------------------------

newtype CurrencySymbol = CurrencySymbol ByteArray

derive newtype instance Eq CurrencySymbol
derive newtype instance FromData CurrencySymbol
derive newtype instance FromMetadata CurrencySymbol
derive newtype instance Ord CurrencySymbol
derive newtype instance ToData CurrencySymbol
derive newtype instance ToMetadata CurrencySymbol

instance Arbitrary CurrencySymbol where
  arbitrary =
    unsafePartial fromJust <<< mkCurrencySymbol <<< byteArrayFromIntArrayUnsafe
      <$> vectorOf 28 (chooseInt 0 255)

instance Show CurrencySymbol where
  show (CurrencySymbol cs) = "(CurrencySymbol " <> show cs <> ")"

-- This is needed for `ApplyArgs`. Plutus has an `unCurrencySymbol` field.
instance DecodeAeson CurrencySymbol where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    ( note (TypeMismatch "Invalid CurrencySymbol") <<< mkCurrencySymbol
        <=< note (TypeMismatch "Invalid ByteArray") <<< hexToByteArray
        <=< flip getField "unCurrencySymbol"
    )

instance EncodeAeson CurrencySymbol where
  encodeAeson (CurrencySymbol ba) = encodeAeson
    { "unCurrencySymbol": byteArrayToHex ba }

getCurrencySymbol :: CurrencySymbol -> ByteArray
getCurrencySymbol (CurrencySymbol curSymbol) = curSymbol

-- Currency symbol for Ada, do not use inside NonAdaAsset map - do not export.
-- For internal use only
unsafeAdaSymbol :: CurrencySymbol
unsafeAdaSymbol = CurrencySymbol mempty

-- | Create a `CurrencySymbol` from a `ByteArray` since `CurrencySymbol` data
-- | constructor is not exported
mkCurrencySymbol :: ByteArray -> Maybe CurrencySymbol
mkCurrencySymbol byteArr =
  scriptHashFromBytes byteArr *> pure (CurrencySymbol byteArr)

-- Do not export. Create an Ada `CurrencySymbol` from a `ByteArray`
mkUnsafeAdaSymbol :: ByteArray -> Maybe CurrencySymbol
mkUnsafeAdaSymbol byteArr =
  if byteArr == mempty then pure unsafeAdaSymbol else Nothing

--------------------------------------------------------------------------------
-- NonAdaAsset
--------------------------------------------------------------------------------

newtype NonAdaAsset = NonAdaAsset (Map CurrencySymbol (Map TokenName BigInt))

derive newtype instance Eq NonAdaAsset

instance Arbitrary NonAdaAsset where
  arbitrary =
    NonAdaAsset <$>
      genMap
        (arbitrary :: Gen CurrencySymbol)
        ( flip suchThat (not Map.isEmpty) $
            genMap
              (arbitrary :: Gen TokenName)
              (fromInt <$> suchThat arbitrary (_ >= one) :: Gen BigInt)
        )

instance Show NonAdaAsset where
  show (NonAdaAsset nonAdaAsset) = "(NonAdaAsset " <> show nonAdaAsset <> ")"

instance Semigroup NonAdaAsset where
  append = unionWithNonAda (+)

instance Monoid NonAdaAsset where
  mempty = NonAdaAsset Map.empty

instance JoinSemilattice NonAdaAsset where
  join = unionWithNonAda max

instance MeetSemilattice NonAdaAsset where
  meet = unionWithNonAda min

instance Negate NonAdaAsset where
  negation = NonAdaAsset <<< map (map negate) <<< unwrapNonAdaAsset

instance Split NonAdaAsset where
  split (NonAdaAsset mp) = NonAdaAsset npos /\ NonAdaAsset pos
    where
    splitIntl
      :: Map TokenName BigInt
      -> These (Map TokenName BigInt) (Map TokenName BigInt)
    splitIntl mp' = Both l r
      where
      l /\ r = mapThese (\i -> if i <= zero then This (negate i) else That i)
        mp'

    npos /\ pos = mapThese splitIntl mp

instance EncodeAeson NonAdaAsset where
  encodeAeson (NonAdaAsset m) = encodeAeson $ encodeMap $ encodeMap <$> m

instance Equipartition NonAdaAsset where
  equipartition nonAdaAssets numParts =
    foldl accumulate (NEArray.replicate numParts mempty)
      (flattenNonAdaValue nonAdaAssets)
    where
    accumulate
      :: NonEmptyArray NonAdaAsset
      -> (CurrencySymbol /\ TokenName /\ BigInt)
      -> NonEmptyArray NonAdaAsset
    accumulate xs (cs /\ tn /\ tokenQuantity) =
      NEArray.zipWith append xs $
        map (mkSingletonNonAdaAsset cs tn)
          (equipartition tokenQuantity numParts)

-- | Partitions a `NonAdaAsset` into smaller `NonAdaAsset`s, where the
-- | quantity of each token is equipartitioned across the resultant
-- | `NonAdaAsset`s, with the goal that no token quantity in any of the
-- | resultant `NonAdaAsset`s exceeds the given upper bound.
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/d4b30de073f2b5eddb25bf12c2453abb42e8b352/lib/wallet/src/Cardano/Wallet/Primitive/Types/TokenMap.hs#L780
equipartitionAssetsWithTokenQuantityUpperBound
  :: NonAdaAsset -> BigInt -> NonEmptyArray NonAdaAsset /\ Int
equipartitionAssetsWithTokenQuantityUpperBound nonAdaAssets maxTokenQuantity =
  case
    maxTokenQuantity <= zero || currentMaxTokenQuantity <= maxTokenQuantity
    of
    true ->
      NEArray.singleton nonAdaAssets /\ one
    false ->
      equipartition nonAdaAssets numParts /\ numParts
  where
  numParts :: Int
  numParts =
    Int.ceil (toNumber currentMaxTokenQuantity / toNumber maxTokenQuantity)

  tokenQuantity :: (CurrencySymbol /\ TokenName /\ BigInt) -> BigInt
  tokenQuantity (_ /\ _ /\ quantity) = quantity

  currentMaxTokenQuantity :: BigInt
  currentMaxTokenQuantity =
    foldl (\quantity tn -> quantity `max` tokenQuantity tn) zero
      (flattenNonAdaValue nonAdaAssets)

unwrapNonAdaAsset :: NonAdaAsset -> Map CurrencySymbol (Map TokenName BigInt)
unwrapNonAdaAsset (NonAdaAsset mp) = mp

-- We shouldn't need this check if we don't export unsafeAdaSymbol etc.
-- | Create a singleton `NonAdaAsset` which by definition should be safe since
-- | `CurrencySymbol` and `TokenName` are safe
mkSingletonNonAdaAsset
  :: CurrencySymbol
  -> TokenName
  -> BigInt
  -> NonAdaAsset
mkSingletonNonAdaAsset curSymbol tokenName amount
  | amount == zero = mempty
  | otherwise =
      NonAdaAsset $ Map.singleton curSymbol $ Map.singleton tokenName amount

-- Assume all CurrencySymbol are well-formed at this point, since they come from
-- mkCurrencySymbol and mkTokenName.
-- | Given the relevant map, create a normalized `NonAdaAsset`.
mkNonAdaAsset :: Map CurrencySymbol (Map TokenName BigInt) -> NonAdaAsset
mkNonAdaAsset = normalizeNonAdaAsset <<< NonAdaAsset

mkNonAdaAssetsFromTokenMap'
  :: forall (t :: Type -> Type)
   . Traversable t
  => t (ByteArray /\ Map TokenName BigInt)
  -> Maybe (Map CurrencySymbol (Map TokenName BigInt))
mkNonAdaAssetsFromTokenMap' =
  traverse (ltraverse mkCurrencySymbol) >>> map Map.fromFoldable

-- | Creates a `NonAdaAsset` from bytearrays and already safely created `TokenName`
-- | map
mkNonAdaAssetsFromTokenMap
  :: forall (t :: Type -> Type)
   . Traversable t
  => t (ByteArray /\ Map TokenName BigInt)
  -> Maybe NonAdaAsset
mkNonAdaAssetsFromTokenMap xs = mkNonAdaAssetsFromTokenMap' xs <#> mkNonAdaAsset

mkNonAdaAssets'
  :: forall (s :: Type -> Type) (t :: Type -> Type)
   . Traversable s
  => Traversable t
  => s (ByteArray /\ t (ByteArray /\ BigInt))
  -> Maybe (Map CurrencySymbol (Map TokenName BigInt))
mkNonAdaAssets' =
  traverse (bitraverse mkCurrencySymbol mkTokenNames) >>> map Map.fromFoldable

-- | Given a `Traversable` of `ByteArray`s and amounts to safely convert into a
-- | `NonAdaAsset`
mkNonAdaAssets
  :: forall (s :: Type -> Type) (t :: Type -> Type)
   . Traversable s
  => Traversable t
  => s (ByteArray /\ t (ByteArray /\ BigInt))
  -> Maybe NonAdaAsset
mkNonAdaAssets xs = mkNonAdaAssets' xs <#> mkNonAdaAsset

getNonAdaAsset :: Value -> NonAdaAsset
getNonAdaAsset (Value _ nonAdaAsset) = nonAdaAsset

-- This is safe assuming we don't export `unsafeAdaSymbol` as user would need to
-- construct `CurrencySymbol` and `TokenName` safely.
getNonAdaAsset' :: Value -> Map CurrencySymbol (Map TokenName BigInt)
getNonAdaAsset' (Value _ (NonAdaAsset nonAdaAsset)) = nonAdaAsset

--------------------------------------------------------------------------------
-- Value
--------------------------------------------------------------------------------
-- | In Plutus, Ada is is stored inside the map (with currency symbol and token
-- | name being empty bytestrings). cardano-serialization-lib makes semantic
-- | distinction between native tokens and Ada, and we follow this convention.
data Value = Value Coin NonAdaAsset

derive instance Generic Value _
derive instance Eq Value

instance Arbitrary Value where
  arbitrary = Value <$> arbitrary <*> arbitrary

instance Show Value where
  show = genericShow

instance Semigroup Value where
  append (Value c1 m1) (Value c2 m2) = Value (c1 <> c2) (m1 <> m2)

instance Monoid Value where
  mempty = Value mempty mempty

instance JoinSemilattice Value where
  join (Value c1 m1) (Value c2 m2) = Value (c1 `join` c2) (m1 `join` m2)

instance MeetSemilattice Value where
  meet (Value c1 m1) (Value c2 m2) = Value (c1 `meet` c2) (m1 `meet` m2)

instance Negate Value where
  negation (Value coin nonAdaAsset) =
    Value (negation coin) (negation nonAdaAsset)

instance Split Value where
  split (Value coin nonAdaAsset) =
    bimap (flip Value mempty) (flip Value mempty) (split coin)
      <> bimap (Value mempty) (Value mempty) (split nonAdaAsset)

instance EncodeAeson Value where
  encodeAeson (Value coin nonAdaAsset) = encodeAeson
    { coin
    , nonAdaAsset
    }

instance Equipartition Value where
  equipartition (Value coin nonAdaAssets) numParts =
    NEArray.zipWith mkValue
      (equipartition coin numParts)
      (equipartition nonAdaAssets numParts)

-- | Partitions a `Value` into smaller `Value`s, where the Ada amount and the
-- | quantity of each token is equipartitioned across the resultant `Value`s,
-- | with the goal that no token quantity in any of the resultant `Value`s
-- | exceeds the given upper bound.
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/d4b30de073f2b5eddb25bf12c2453abb42e8b352/lib/wallet/src/Cardano/Wallet/Primitive/Types/TokenBundle.hs#L381
equipartitionValueWithTokenQuantityUpperBound
  :: BigInt -> Value -> NonEmptyArray Value
equipartitionValueWithTokenQuantityUpperBound maxTokenQuantity value =
  let
    Value coin nonAdaAssets = value
    ms /\ numParts =
      equipartitionAssetsWithTokenQuantityUpperBound nonAdaAssets
        maxTokenQuantity
  in
    NEArray.zipWith mkValue (equipartition coin numParts) ms

-- | Create a `Value` from `Coin` and `NonAdaAsset`, the latter should have been
-- | constructed safely at this point.
mkValue :: Coin -> NonAdaAsset -> Value
mkValue = Value

-- | Creates a singleton value given two byte arrays for currency symbol and
-- | token name respectively
mkSingletonValue :: ByteArray -> ByteArray -> BigInt -> Maybe Value
mkSingletonValue curSymbol' tokenName' amount = do
  curSymbol <- mkCurrencySymbol curSymbol' <|> mkUnsafeAdaSymbol curSymbol'
  tokenName <- mkTokenName tokenName'
  mkSingletonValue' curSymbol tokenName amount

-- Similar to `mkSingletonValue` but the user has a `CurrencySymbol` and `TokenName`
-- at hand. This could be exported (and used only for `NonAdaAsset`) or internally
-- for both `Coin` and `NonAdaAsset`.
mkSingletonValue' :: CurrencySymbol -> TokenName -> BigInt -> Maybe Value
mkSingletonValue' curSymbol tokenName amount = do
  let isAdaCs = curSymbol == unsafeAdaSymbol
  -- Can't have a non-empty TokenName with Ada CurrencySymbol. I.e. It's either
  -- not an Ada currency symbol (with any valid TokenName) or an Ada currency
  -- symbol with an empty TokenName
  guard $ not isAdaCs || (isAdaCs && tokenName == adaToken)
  pure
    if isAdaCs then Value (Coin amount) mempty
    else Value mempty $ mkSingletonNonAdaAsset curSymbol tokenName amount

--------------------------------------------------------------------------------
-- AssetClass
--------------------------------------------------------------------------------

data AssetClass = AssetClass CurrencySymbol TokenName

derive instance Generic AssetClass _
derive instance Eq AssetClass
derive instance Ord AssetClass

instance Arbitrary AssetClass where
  arbitrary = AssetClass <$> arbitrary <*> arbitrary

instance Show AssetClass where
  show = genericShow

assetToValue :: AssetClass -> BigInt -> Value
assetToValue (AssetClass cs tn) quantity =
  mkValue mempty (mkSingletonNonAdaAsset cs tn quantity)

getAssetQuantity :: AssetClass -> Value -> BigInt
getAssetQuantity (AssetClass cs tn) value = valueOf value cs tn

valueAssets :: Value -> Array (AssetClass /\ BigInt)
valueAssets (Value _ assets) =
  Array.fromFoldable (flattenNonAdaValue assets)
    <#> \(cs /\ tn /\ quantity) -> AssetClass cs tn /\ quantity

valueAssetClasses :: Value -> Array AssetClass
valueAssetClasses = map fst <<< valueAssets

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Normalize `NonAdaAsset` so that it doesn't contain zero-valued tokens.
normalizeNonAdaAsset :: NonAdaAsset -> NonAdaAsset
normalizeNonAdaAsset = filterNonAdaAsset (notEq zero)

posNonAdaAsset :: NonAdaAsset -> NonAdaAsset
posNonAdaAsset = filterNonAdaAsset (\x -> x > zero)

filterNonAdaAsset :: (BigInt -> Boolean) -> NonAdaAsset -> NonAdaAsset
filterNonAdaAsset p (NonAdaAsset mp) =
  NonAdaAsset $ Map.filter (not Map.isEmpty) $ Map.filter p <$> mp

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/src/PlutusTx.AssocMap.html#union
-- | Combine two `Map`s.
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
-- | Combine two `NonAdaAsset` maps
unionNonAda
  :: NonAdaAsset
  -> NonAdaAsset
  -> Map CurrencySymbol (Map TokenName (These BigInt BigInt))
unionNonAda (NonAdaAsset l) (NonAdaAsset r) =
  let
    combined
      :: Map CurrencySymbol
           (These (Map TokenName BigInt) (Map TokenName BigInt))
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
-- | Same as `unionWith` but specifically for `NonAdaAsset`
unionWithNonAda
  :: (BigInt -> BigInt -> BigInt)
  -> NonAdaAsset
  -> NonAdaAsset
  -> NonAdaAsset
unionWithNonAda f ls rs =
  let
    combined :: Map CurrencySymbol (Map TokenName (These BigInt BigInt))
    combined = unionNonAda ls rs

    unBoth :: These BigInt BigInt -> BigInt
    unBoth k' = case k' of
      This a -> f a zero
      That b -> f zero b
      Both a b -> f a b
  in
    normalizeNonAdaAsset $
      NonAdaAsset (map unBoth <$> combined)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionWith
-- | Combines `Value` with a binary function on `BigInt`s.
unionWith
  :: (BigInt -> BigInt -> BigInt)
  -> Value
  -> Value
  -> Value
unionWith f (Value (Coin c) na) (Value (Coin c') na') =
  Value (Coin $ f c c') (unionWithNonAda f na na')

-- Based on https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#flattenValue
-- Flattens non-Ada Value into a list
flattenNonAdaValue
  :: NonAdaAsset -> List (CurrencySymbol /\ TokenName /\ BigInt)
flattenNonAdaValue (NonAdaAsset nonAdaAsset) = do
  cs /\ m <- toUnfoldable nonAdaAsset
  tn /\ a <- toUnfoldable m
  guard $ a /= zero
  pure $ cs /\ tn /\ a

-- Flattens Value guarding against zeros
unsafeFlattenValue :: Value -> List (CurrencySymbol /\ TokenName /\ BigInt)
unsafeFlattenValue (Value coin@(Coin lovelaces) nonAdaAsset) =
  let
    flattenedNonAda :: List (CurrencySymbol /\ TokenName /\ BigInt)
    flattenedNonAda = flattenNonAdaValue nonAdaAsset
  in
    case coin == mempty of
      true -> flattenedNonAda
      false -> (unsafeAdaSymbol /\ adaToken /\ lovelaces) : flattenedNonAda

-- From https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- Converts a single tuple to Value
unflattenValue :: CurrencySymbol /\ TokenName /\ BigInt -> Maybe Value
unflattenValue (curSymbol /\ tokenName /\ amount) =
  mkSingletonValue' curSymbol tokenName amount

-- | Predicate on whether some `Value` contains Ada only.
isAdaOnly :: Value -> Boolean
isAdaOnly v =
  case unsafeFlattenValue v of
    (cs /\ tn /\ _) : Nil ->
      cs == unsafeAdaSymbol &&
        tn == adaToken
    _ -> false

minus :: Value -> Value -> Value
minus lhs rhs =
  let
    negativeValues :: List (CurrencySymbol /\ TokenName /\ BigInt)
    negativeValues = unsafeFlattenValue rhs <#>
      (\(c /\ t /\ a) -> c /\ t /\ negate a)
  in
    unsafePartial $
      lhs <> fold (fromJust $ traverse unflattenValue negativeValues)

-- From https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- "isValueNat" uses unsafeFlattenValue which guards against zeros, so non-strict
-- inequality is redundant. So we use strict equality instead.
isPos :: Value -> Boolean
isPos = all (\(_ /\ _ /\ a) -> a > zero) <<< unsafeFlattenValue

-- From https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#isZero
-- | Check whether a `Value` is zero.
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
-- Check whether a binary relation holds for value pairs of two `Value` maps,
-- supplying 0 where a key is only present in one of them.
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
-- | Check whether one `Value` is greater than or equal to another. See `Value` for an explanation of how operations on `Value`s work.
geq :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
geq = checkBinRel (>=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#gt
-- | Check whether one `Value` is strictly greater than another. See `Value` for an explanation of how operations on `Value`s work.
gt :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
gt l r = not (isZero l && isZero r) && checkBinRel (>) l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#leq
-- | Check whether one `Value` is less than or equal to another. See `Value` for an explanation of how operations on `Value`s work.
leq :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
leq = checkBinRel (<=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#lt
-- | Check whether one `Value` is strictly less than another. See `Value` for an explanation of how operations on `Value`s work.
lt :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
lt l r = not (isZero l && isZero r) && checkBinRel (<) l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#eq
-- | Check whether one `Value` is equal to another. See `Value` for an explanation of how operations on `Value`s work.
eq :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
eq = checkBinRel (==)

unsafeIsAda :: CurrencySymbol -> TokenName -> Boolean
unsafeIsAda curSymbol tokenName =
  curSymbol == unsafeAdaSymbol &&
    tokenName == adaToken

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#valueOf
-- | Get the quantity of the given currency in the `Value`.
valueOf :: Value -> CurrencySymbol -> TokenName -> BigInt
valueOf (Value (Coin lovelaces) (NonAdaAsset nonAdaAsset)) curSymbol tokenName =
  case unsafeIsAda curSymbol tokenName of
    false ->
      case lookup curSymbol nonAdaAsset of
        Nothing -> zero
        Just i -> case lookup tokenName i of
          Nothing -> zero
          Just v -> v
    true -> lovelaces

-- | The number of distinct non-Ada assets.
numNonAdaAssets :: Value -> BigInt
numNonAdaAssets (Value _ nonAdaAssets) =
  length (flattenNonAdaValue nonAdaAssets)

-- | The number of distinct non-Ada currency symbols, i.e. the number of policy
-- | IDs not including Ada in `Coin`.
numNonAdaCurrencySymbols :: Value -> BigInt
numNonAdaCurrencySymbols (Value _ nonAdaAssets) =
  fromInt <<< length <<< List.nubByEq ((==) `on` fst) $
    flattenNonAdaValue nonAdaAssets

-- | The number of distinct currency symbols, i.e. the number of policy IDs
-- | including Ada in `Coin`.
numCurrencySymbols :: Value -> BigInt
numCurrencySymbols value@(Value coin _)
  | coin == mempty =
      numNonAdaCurrencySymbols value
  | otherwise =
      one + numNonAdaCurrencySymbols value

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

-- Don't export as we don't to expose tokenNames although may be it's okay
-- given `mkTokenName` doesn't need to be `Maybe`.
unsafeAllTokenNames :: Value -> Set TokenName
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
filterNonAda (Value _ nonAda) = Value mempty nonAda

-- I think this is safe because a CurrencySymbol can only be constructed by
-- checking scriptHashFromBytes so it must be a valid ScriptHash too. Otherwise
-- we'd have a Maybe context from scriptHashFromBytes again from something we
-- already know is a valid CurrencySymbol
currencyScriptHash :: CurrencySymbol -> ScriptHash
currencyScriptHash (CurrencySymbol byteArray) =
  unsafePartial fromJust $ scriptHashFromBytes byteArray

scriptHashAsCurrencySymbol :: ScriptHash -> CurrencySymbol
scriptHashAsCurrencySymbol = CurrencySymbol <<< unwrap <<< scriptHashToBytes

-- | The minting policy hash of a currency symbol
currencyMPSHash :: CurrencySymbol -> MintingPolicyHash
currencyMPSHash = MintingPolicyHash <<< currencyScriptHash

-- We haven't provided any safety on MintingPolicyHash, analagous to
-- CurrencySymbol, so we need Maybe context. We could remove Maybe if we do.
-- Plutus doesn't use Maybe here.
-- | The currency symbol of a monetary policy hash
mpsSymbol :: MintingPolicyHash -> Maybe CurrencySymbol
mpsSymbol (MintingPolicyHash h) = mkCurrencySymbol $ unwrap $ scriptHashToBytes
  h

-- Like `mapEither` that works with 'These'.
mapThese
  :: forall (a :: Type) (b :: Type) (k :: Type) (v :: Type)
   . Ord k
  => (v -> These a b)
  -> Map k v
  -> Map k a /\ Map k b
mapThese f mps =
  bimap Map.fromFoldable Map.fromFoldable $ foldrWithIndex f' ([] /\ []) mps'
  where
  mps' :: Map k (These a b)
  mps' = map f mps

  f'
    :: k
    -> These a b
    -> Array (k /\ a) /\ Array (k /\ b)
    -> Array (k /\ a) /\ Array (k /\ b)
  f' k v (as /\ bs) = case v of
    This a -> (k /\ a) `cons` as /\ bs
    That b -> as /\ (k /\ b) `cons` bs
    Both a b -> (k /\ a) `cons` as /\ (k /\ b) `cons` bs
