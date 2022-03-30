-- | A module that defines tokens in Cardano and helpers. The representation of
-- | `Value` is isomorphic to Plutus' reprsentation with the only difference
-- | being that Ada's currency symbol does not exist, instead, Ada is
-- | represented by `Coin`.
module Contract.Value
  ( module Value
  , scriptCurrencySymbol
  ) where

import Prelude
import Contract.Monad (Contract)
import Data.Argonaut (class DecodeJson)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Data.Tuple.Nested (type (/\))
import QueryM (getAssetMetadata) as QueryM
import Scripts (scriptCurrencySymbol) as Scripts
import Types.Scripts (MintingPolicy)
import Types.Value
  ( Coin(Coin)
  , CurrencySymbol
  , NonAdaAsset(NonAdaAsset)
  , TokenName
  , Value(Value)
  , adaToken
  , coinToValue
  , currencyMPSHash
  , eq
  , filterNonAda
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
  , mkSingletonValue'
  , mkTokenName
  , mkTokenNames
  , mkValue
  , mpsSymbol
  , negation
  , numCurrencySymbols
  , numTokenNames
  , unionWith
  , valueOf
  , valueToCoin
  , valueToCoin'
  ) as Value

scriptCurrencySymbol :: MintingPolicy -> Contract (Maybe Value.CurrencySymbol)
scriptCurrencySymbol = wrap <<< Scripts.scriptCurrencySymbol

getAssetMetadata
  :: forall (a :: Type)
   . DecodeJson a
  => Value.CurrencySymbol /\ Value.TokenName
  -> Contract (Maybe a)
getAssetMetadata = wrap <<< map hush <<< QueryM.getAssetMetadata
