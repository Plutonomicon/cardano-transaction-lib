module Contract.Value
  ( module X
  , CurrencySymbol
  , TokenName
  ) where

import Cardano.Types (AssetName, ScriptHash)
import Cardano.Types.AssetName (AssetName(AssetName)) as X
import Cardano.Types.ScriptHash (ScriptHash(ScriptHash)) as X
import Cardano.Types.Value
  ( Value(Value)
  , add
  , assetToValue
  , checkBinRel
  , checkPred
  , coinToValue
  , empty
  , fromCsl
  , geq
  , getAssetQuantity
  , getCoin
  , getMultiAsset
  , gt
  , isPositive
  , isZero
  , leq
  , lovelaceValueOf
  , lt
  , minus
  , mkValue
  , pprintValue
  , singleton
  , sum
  , toCsl
  , unionWith
  , valueAssetClasses
  , valueAssets
  , valueOf
  , valueToCoin
  ) as X

-- | Deprecated, use `Cardano.Types.ScriptHash`
type CurrencySymbol = ScriptHash

-- | Deprecated, use `Cardano.Types.AssetName`
type TokenName = AssetName
