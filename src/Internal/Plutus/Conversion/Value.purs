module Ctl.Internal.Plutus.Conversion.Value
  ( fromPlutusValue
  , toPlutusValue
  ) where

import Prelude

import Cardano.Types.AsCbor (encodeCbor)
import Cardano.Types.AssetName (unAssetName)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.MultiAsset (MultiAsset(MultiAsset))
import Cardano.Types.MultiAsset as MultiAsset
import Ctl.Internal.Cardano.Types.Value (Coin(Coin), Value(Value)) as Cardano
import Ctl.Internal.Cardano.Types.Value (MultiAsset, mkValue)
import Ctl.Internal.Helpers (notImplemented)
import Ctl.Internal.Plutus.Types.AssocMap (lookup) as Plutus.AssocMap
import Ctl.Internal.Plutus.Types.CurrencySymbol (adaSymbol, getCurrencySymbol) as Plutus
import Ctl.Internal.Plutus.Types.Value (Value) as Plutus
import Ctl.Internal.Plutus.Types.Value (adaToken)
import Ctl.Internal.Plutus.Types.Value (getValue, lovelaceValueOf, singleton') as Plutus.Value
import Data.Array (head, partition)
import Data.Foldable (fold)
import Data.List (List)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Literals.Undefined (undefined)
import Partial.Unsafe (unsafePartial)

-- The underlying `Plutus.Types.AssocMap` of `Plutus.Types.Value` doesn't
-- have the `Ord` constraint on the keys. Therefore, one should be careful when
-- performing conversions between `Value`s, since the ordering of components
-- can't be guaranteed.

--------------------------------------------------------------------------------
-- Plutus Value -> Types.Value
--------------------------------------------------------------------------------

fromPlutusValue :: Plutus.Value -> Maybe Cardano.Value
fromPlutusValue plutusValue = notImplemented

-- mkValue adaCoin nonAdaAssets
-- where
-- { adaTokenMap, nonAdaTokenMap } =
--   (\x -> { adaTokenMap: x.yes, nonAdaTokenMap: x.no }) <<<
--     partition (\(cs /\ _) -> cs == Plutus.adaSymbol) $
--     (unwrap $ Plutus.Value.getValue plutusValue)

-- adaValue :: Cardano.Coin
-- adaValue = fromMaybe zero do
--   adaTokens <- snd <$> head adaTokenMap
--   Plutus.AssocMap.lookup adaToken adaTokens

-- nonAdaAssets :: Maybe MultiAsset
-- nonAdaAssets =
--   MultiAsset
--   $ nonAdaTokenMap <#> \(cs /\ tokens) ->
--       Plutus.getCurrencySymbol cs /\ Map.fromFoldable (unwrap tokens)

--------------------------------------------------------------------------------
-- Cardano.Value -> Plutus Value
--------------------------------------------------------------------------------

toPlutusValue :: Cardano.Value -> Plutus.Value
toPlutusValue (Cardano.Value (Cardano.Coin adaAmount) nonAdaAssets) =
  adaValue <> fold nonAdaValues
  where
  adaValue :: Plutus.Value
  adaValue
    | adaAmount == BigNum.zero = mempty
    | otherwise = Plutus.Value.lovelaceValueOf $ BigNum.toBigInt adaAmount

  nonAdaValues :: Array Plutus.Value
  nonAdaValues =
    MultiAsset.flatten nonAdaAssets <#> \(cs /\ tn /\ val) ->
      unsafePartial fromJust
        $ Plutus.Value.singleton' (unwrap $ encodeCbor cs) (unAssetName tn)
        $ BigNum.toBigInt val
