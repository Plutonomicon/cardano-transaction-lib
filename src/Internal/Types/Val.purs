module Ctl.Internal.Types.Val where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types
  ( AssetClass(AssetClass)
  , AssetName
  , BigInt
  , Coin
  , MultiAsset(MultiAsset)
  , ScriptHash
  , Value(Value)
  )
import Cardano.Types.AssetName (fromAssetName)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint (Mint)
import Cardano.Types.Mint as Mint
import Cardano.Types.MultiAsset as MultiAsset
import Data.Array (cons, foldr)
import Data.Bifunctor (bimap)
import Data.ByteArray (byteArrayToHex)
import Data.Foldable (all)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lattice (class JoinSemilattice, class MeetSemilattice)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.These (These(This, That, Both), these)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import JS.BigInt as BigInt

-- | A long-integer-backed Value that can be negative and does not overflow.
-- | Used in CTL internally instead of the ledger-style `Value` type to simplify
-- | overflow handling.
data Val = Val BigInt ValAssets

instance Eq Val where
  eq = checkBinRel (==)

instance Semigroup Val where
  append (Val a ma) (Val b mb) = Val (a + b) (unionWithNonAda add ma mb)

instance Monoid Val where
  mempty = Val zero Map.empty

derive instance Generic Val _

instance Show Val where
  show = genericShow

instance JoinSemilattice Val where
  join (Val c1 m1) (Val c2 m2) = Val (c1 `max` c2) (m1 `unionWithNonAda max` m2)

instance MeetSemilattice Val where
  meet (Val c1 m1) (Val c2 m2) = Val (c1 `min` c2) (m1 `unionWithNonAda min` m2)

-- | Split a value into its positive and non-positive parts. The first element of
-- | the tuple contains the non-positive parts of the value, the second element
-- | contains the positive parts. The convention is non-positive parts are
-- | negated to make them positive in the output.
class Split (a :: Type) where
  split :: a -> a /\ a

instance Split BigInt where
  split c =
    if c <= zero then negate c /\ zero else zero /\ c

instance Split Val where
  split (Val c1 mp) = Val np npos /\ Val p pos
    where
    np /\ p = split c1

    splitIntl
      :: Map AssetName BigInt
      -> These (Map AssetName BigInt) (Map AssetName BigInt)
    splitIntl mp' = Both l r
      where
      l /\ r = mapThese (\i -> if i <= zero then This (negate i) else That i)
        mp'

    npos /\ pos = mapThese splitIntl mp

type ValAssets = Map ScriptHash (Map AssetName BigInt)

leq :: Val -> Val -> Boolean
leq = checkBinRel (<=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkBinRel
-- Check whether a binary relation holds for value pairs of two `Value` maps,
-- supplying 0 where a key is only present in one of them.
checkBinRel :: (BigInt -> BigInt -> Boolean) -> Val -> Val -> Boolean
checkBinRel f l r =
  let
    unThese :: These BigInt BigInt -> Boolean
    unThese k' = case k' of
      This a -> f a zero
      That b -> f zero b
      Both a b -> f a b
  in
    checkPred unThese l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkPred
checkPred :: (These BigInt BigInt -> Boolean) -> Val -> Val -> Boolean
checkPred f (Val l ls) (Val r rs) =
  let
    inner :: Map AssetName (These BigInt BigInt) -> Boolean
    inner = all f -- this "all" may need to be checked?
  in
    f (Both l r) && all inner (unionNonAda ls rs) -- this "all" may need to be checked?

getCoin :: Val -> BigInt
getCoin (Val c _) = c

getAssets :: Val -> Map ScriptHash (Map AssetName BigInt)
getAssets (Val _ ma) = ma

getAssetQuantity :: AssetClass -> Val -> BigInt
getAssetQuantity (AssetClass sh tn) val =
  fromMaybe zero $ Map.lookup sh (getAssets val) >>= Map.lookup tn

valueAssets :: Val -> Array (AssetClass /\ BigInt)
valueAssets (Val _ ma) =
  Map.toUnfoldable ma >>= \(scriptHash /\ mp) ->
    Map.toUnfoldable mp >>= \(tokenName /\ amount) ->
      [ AssetClass scriptHash tokenName /\ amount ]

pprintVal :: Val -> TagSet
pprintVal (Val coin ma) = TagSet.fromArray $
  [ "Lovelace" `tag` BigInt.toString coin ]
    <>
      if ma /= Map.empty then
        [ "Assets" `tagSetTag` pprintMultiAsset ma ]
      else []
  where

  pprintMultiAsset :: Map ScriptHash (Map AssetName BigInt) -> TagSet
  pprintMultiAsset mp = TagSet.fromArray $
    Map.toUnfoldable mp <#> \(scriptHash /\ tokens) ->
      byteArrayToHex (unwrap $ encodeCbor scriptHash) `tagSetTag`
        TagSet.fromArray
          ( Map.toUnfoldable tokens <#> \(assetName /\ amount) ->
              fromAssetName byteArrayToHex show assetName `tag` BigInt.toString
                amount
          )

fromCoin :: Coin -> Val
fromCoin = flip Val Map.empty <<< BigNum.toBigInt <<< unwrap

fromValue :: Value -> Val
fromValue (Value coin ma) = Val (BigNum.toBigInt $ unwrap coin)
  $ map (map BigNum.toBigInt)
  $ unwrap ma

toMultiAsset :: Map ScriptHash (Map AssetName BigInt) -> Maybe MultiAsset
toMultiAsset = map (map wrap) $ traverse $ traverse $ BigNum.fromBigInt

toValue :: Val -> Maybe Value
toValue (Val coin ma) = do
  ma' <- toMultiAsset ma
  coin' <- BigNum.fromBigInt coin
  pure $ Value (wrap coin') ma'

fromMultiAsset :: MultiAsset -> Val
fromMultiAsset (MultiAsset ma) = Val zero $ map (map BigNum.toBigInt) ma

fromMint :: Mint -> Val
fromMint mint =
  Val zero $ foldr insert Map.empty $ Mint.flatten mint
  where
  insert
    :: (ScriptHash /\ AssetName /\ Int.Int)
    -> Map ScriptHash (Map AssetName BigInt)
    -> Map ScriptHash (Map AssetName BigInt)
  insert (scriptHash /\ assetName /\ amount) ma =
    map joinAssets $
      MultiAsset.union
        ma
        ( Map.singleton scriptHash $ Map.singleton assetName $ Int.toBigInt
            amount
        )

  joinAssets
    :: These (Map AssetName BigInt) (Map AssetName BigInt)
    -> Map AssetName BigInt
  joinAssets = these identity identity
    (\x y -> map unBoth $ MultiAsset.union x y)
  unBoth (Both a b) = add a b
  unBoth (This a) = a
  unBoth (That b) = b

minus :: Val -> Val -> Val
minus (Val a ma) (Val b mb) = Val (a - b) (unionWithNonAda (-) ma mb)

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

unionWithNonAda
  :: (BigInt -> BigInt -> BigInt)
  -> Map ScriptHash (Map AssetName BigInt)
  -> Map ScriptHash (Map AssetName BigInt)
  -> Map ScriptHash (Map AssetName BigInt)
unionWithNonAda f ls rs =
  let
    combined :: Map ScriptHash (Map AssetName (These BigInt BigInt))
    combined = unionNonAda ls rs

    unBoth :: These BigInt BigInt -> BigInt
    unBoth k' = case k' of
      This a -> f a zero
      That b -> f zero b
      Both a b -> f a b
  in
    map (map unBoth) combined

unionNonAda
  :: Map ScriptHash (Map AssetName BigInt)
  -> Map ScriptHash (Map AssetName BigInt)
  -> Map ScriptHash (Map AssetName (These BigInt BigInt))
unionNonAda l r =
  let
    combined
      :: Map ScriptHash
           (These (Map AssetName BigInt) (Map AssetName BigInt))
    combined = MultiAsset.union l r

    unBoth
      :: These (Map AssetName BigInt) (Map AssetName BigInt)
      -> Map AssetName (These BigInt BigInt)
    unBoth k = case k of
      This a -> This <$> a
      That b -> That <$> b
      Both a b -> MultiAsset.union a b
  in
    unBoth <$> combined
