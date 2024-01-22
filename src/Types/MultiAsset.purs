module Cardano.Types.MultiAsset where

import Prelude hiding (add)

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson)
import Cardano.Serialization.Lib (packMapContainer, unpackMapContainer)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (encodeCbor)
import Cardano.Types.AssetName (AssetName, fromAssetName)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Ctl.Internal.Helpers (decodeMap, encodeMap)
import Ctl.Internal.Partition (class Equipartition, equipartition)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Data.Array (filter)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (replicate, zipWith) as NEArray
import Data.ByteArray (byteArrayToHex)
import Data.Foldable (any, foldM, foldl)
import Data.Lattice (class JoinSemilattice, class MeetSemilattice)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Gen (genMap)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Strong ((***))
import Data.These (These(Both, That, This))
import Data.Traversable (for, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, suchThat)

newtype MultiAsset = MultiAsset (Map ScriptHash (Map AssetName BigNum))

derive newtype instance Eq MultiAsset
derive instance Newtype MultiAsset _

instance Arbitrary MultiAsset where
  arbitrary =
    MultiAsset <$>
      genMap
        (arbitrary :: Gen ScriptHash)
        ( flip suchThat (not Map.isEmpty) $
            genMap
              (arbitrary :: Gen AssetName)
              (BigNum.fromInt <$> suchThat arbitrary (_ >= one) :: Gen BigNum)
        )

instance Show MultiAsset where
  show (MultiAsset nonAdaAsset) = "(MultiAsset " <> show nonAdaAsset <> ")"

instance JoinSemilattice MultiAsset where
  -- `max` can't overflow
  join a b = unsafePartial $ fromJust $ unionWithNonAda (map Just <<< max) a b

instance MeetSemilattice MultiAsset where
  -- `min` can't overflow
  meet a b = unsafePartial $ fromJust $ unionWithNonAda (map Just <<< min) a b

instance EncodeAeson MultiAsset where
  encodeAeson (MultiAsset m) = encodeAeson $ encodeMap $ encodeMap <$> m

instance DecodeAeson MultiAsset where
  decodeAeson aeson = do
    mapAesons <- decodeMap aeson
    MultiAsset <$> for mapAesons decodeMap

instance Equipartition MultiAsset where
  equipartition nonAdaAssets numParts =
    foldl accumulate (NEArray.replicate numParts empty)
      (flatten nonAdaAssets)
    where
    append' a b = unsafePartial $ fromJust $ add a b
    accumulate
      :: NonEmptyArray MultiAsset
      -> (ScriptHash /\ AssetName /\ BigNum)
      -> NonEmptyArray MultiAsset
    accumulate xs (cs /\ tn /\ tokenQuantity) =
      NEArray.zipWith append' xs $
        map (singleton cs tn)
          (equipartition tokenQuantity numParts)

empty :: MultiAsset
empty = MultiAsset Map.empty

add :: MultiAsset -> MultiAsset -> Maybe MultiAsset
add = unionWithNonAda BigNum.add

flatten :: MultiAsset -> Array (ScriptHash /\ AssetName /\ BigNum)
flatten (MultiAsset mp) =
  Map.toUnfoldable mp >>= \(sh /\ mp') -> do
    Map.toUnfoldable mp' >>= \(tn /\ amount) -> pure (sh /\ tn /\ amount)

unflatten :: Array (ScriptHash /\ AssetName /\ BigNum) -> Maybe MultiAsset
unflatten =
  foldM accumulate empty
  where
    uncurry2 f (a /\ b /\ c) = f a b c
    accumulate ma = unionWithNonAda BigNum.add ma <<< uncurry2 singleton

singleton :: ScriptHash -> AssetName -> BigNum -> MultiAsset
singleton sh tn amount = MultiAsset $ Map.singleton sh $ Map.singleton tn amount

pprintMultiAsset :: MultiAsset -> TagSet
pprintMultiAsset mp = TagSet.fromArray $
  Map.toUnfoldable (unwrap mp) <#> \(scriptHash /\ tokens) ->
    byteArrayToHex (unwrap $ encodeCbor scriptHash) `tagSetTag` TagSet.fromArray
      ( Map.toUnfoldable tokens <#> \(assetName /\ amount) ->
          fromAssetName byteArrayToHex show assetName `tag` BigNum.toString
            amount
      )

-- | Normalize `MultiAsset` so that it doesn't contain zero-valued tokens.
normalizeMultiAsset :: MultiAsset -> MultiAsset
normalizeMultiAsset = filterMultiAsset (notEq BigNum.zero)

filterMultiAsset :: (BigNum -> Boolean) -> MultiAsset -> MultiAsset
filterMultiAsset p (MultiAsset mp) =
  MultiAsset $ Map.filter (not Map.isEmpty) $ Map.filter p <$> mp

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionWith
-- | Same as `unionWith` but specifically for `MultiAsset`
unionWithNonAda
  :: (BigNum -> BigNum -> Maybe BigNum)
  -> MultiAsset
  -> MultiAsset
  -> Maybe MultiAsset
unionWithNonAda f ls rs =
  let
    combined :: Map ScriptHash (Map AssetName (These BigNum BigNum))
    combined = unionNonAda ls rs

    unBoth :: These BigNum BigNum -> Maybe BigNum
    unBoth k' = case k' of
      This a -> f a BigNum.zero
      That b -> f BigNum.zero b
      Both a b -> f a b
  in
    normalizeMultiAsset <<< MultiAsset <$> traverse (traverse unBoth) combined

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionVal
-- | Combine two `MultiAsset` maps
unionNonAda
  :: MultiAsset
  -> MultiAsset
  -> Map ScriptHash (Map AssetName (These BigNum BigNum))
unionNonAda (MultiAsset l) (MultiAsset r) =
  let
    combined
      :: Map ScriptHash
           (These (Map AssetName BigNum) (Map AssetName BigNum))
    combined = union l r

    unBoth
      :: These (Map AssetName BigNum) (Map AssetName BigNum)
      -> Map AssetName (These BigNum BigNum)
    unBoth k = case k of
      This a -> This <$> a
      That b -> That <$> b
      Both a b -> union a b
  in
    unBoth <$> combined

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

toCsl :: MultiAsset -> Csl.MultiAsset
toCsl (MultiAsset mp) = packMapContainer $ map (unwrap *** assetsToCsl) $ Map.toUnfoldable mp
  where
    assetsToCsl :: Map AssetName BigNum -> Csl.Assets
    assetsToCsl assets = packMapContainer $ map (unwrap *** unwrap) $ Map.toUnfoldable assets

fromCsl :: Csl.MultiAsset -> MultiAsset
fromCsl multiAsset = MultiAsset $ Map.fromFoldable $
  unpackMapContainer multiAsset <#> wrap *** \asset ->
  Map.fromFoldable (unpackMapContainer asset <#> wrap *** wrap)
