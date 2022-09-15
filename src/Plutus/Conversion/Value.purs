module CTL.Plutus.Conversion.Value
  ( fromPlutusValue
  , toPlutusValue
  ) where

import Prelude

import CTL.Internal.Cardano.Types.Value (Coin(Coin), Value(Value)) as Types
import CTL.Internal.Cardano.Types.Value
  ( NonAdaAsset
  , flattenNonAdaValue
  , getCurrencySymbol
  , mkNonAdaAssetsFromTokenMap
  , mkValue
  )
import CTL.Internal.Types.TokenName (adaToken, getTokenName)
import CTL.Plutus.Types.AssocMap (lookup) as Plutus.AssocMap
import CTL.Plutus.Types.CurrencySymbol (adaSymbol, getCurrencySymbol) as Plutus
import CTL.Plutus.Types.Value (Value) as Plutus
import CTL.Plutus.Types.Value
  ( getValue
  , lovelaceValueOf
  , singleton'
  ) as Plutus.Value
import Data.Array (head, partition)
import Data.Foldable (fold)
import Data.List (List)
import Data.Map (fromFoldable) as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)

-- The underlying `Plutus.Types.AssocMap` of `Plutus.Types.Value` doesn't
-- have the `Ord` constraint on the keys. Therefore, one should be careful when
-- performing conversions between `Value`s, since the ordering of components
-- can't be guaranteed.

--------------------------------------------------------------------------------
-- Plutus Value -> Types.Value
--------------------------------------------------------------------------------

fromPlutusValue :: Plutus.Value -> Types.Value
fromPlutusValue plutusValue =
  adaValue <> mkValue mempty nonAdaAssets
  where
  { adaTokenMap, nonAdaTokenMap } =
    (\x -> { adaTokenMap: x.yes, nonAdaTokenMap: x.no }) <<<
      partition (\(cs /\ _) -> cs == Plutus.adaSymbol) $
      (unwrap $ Plutus.Value.getValue plutusValue)

  adaValue :: Types.Value
  adaValue = flip mkValue mempty <<< wrap <<< fromMaybe zero $ do
    adaTokens <- snd <$> head adaTokenMap
    Plutus.AssocMap.lookup adaToken adaTokens

  nonAdaAssets :: NonAdaAsset
  nonAdaAssets = unsafePartial $ fromJust
    $ mkNonAdaAssetsFromTokenMap
    $ nonAdaTokenMap <#> \(cs /\ tokens) ->
        Plutus.getCurrencySymbol cs /\ Map.fromFoldable (unwrap tokens)

--------------------------------------------------------------------------------
-- Types.Value -> Plutus Value
--------------------------------------------------------------------------------

toPlutusValue :: Types.Value -> Plutus.Value
toPlutusValue (Types.Value (Types.Coin adaAmount) nonAdaAssets) =
  adaValue <> fold nonAdaValues
  where
  adaValue :: Plutus.Value
  adaValue
    | adaAmount == zero = mempty
    | otherwise = Plutus.Value.lovelaceValueOf adaAmount

  nonAdaValues :: List Plutus.Value
  nonAdaValues =
    flattenNonAdaValue nonAdaAssets <#> \(cs /\ tn /\ val) ->
      unsafePartial fromJust $
        Plutus.Value.singleton' (getCurrencySymbol cs) (getTokenName tn) val
