module Ctl.Internal.Types.Val where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types
  ( AssetClass(..)
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
import Cardano.Types.Mint (Mint(Mint))
import Cardano.Types.MultiAsset as MultiAsset
import Data.ByteArray (byteArrayToHex)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.These (These(This, That, Both))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import JS.BigInt as BigInt

data Val = Val BigInt (Map ScriptHash (Map AssetName BigInt))

instance Eq Val where
  eq a b =
    -- the second part is to check for empty outer Map
    toValue a == toValue b && valueAssets a == valueAssets b

instance Semigroup Val where
  append (Val a ma) (Val b mb) = Val (a + b) (unionWithNonAda add ma mb)

instance Monoid Val where
  mempty = Val zero Map.empty

getAssets :: Val -> Map ScriptHash (Map AssetName BigInt)
getAssets (Val _ ma) = ma

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
fromMint (Mint ma) = Val zero $ map (map Int.toBigInt) $ ma

minus :: Val -> Val -> Val
minus (Val a ma) (Val b mb) = Val (a - b) (unionWithNonAda (-) ma mb)

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
