-- | A module that defines tokens in Cardano and helpers. The representation of
-- | `Value` is isomorphic to Plutus' reprsentation with the only difference
-- | being that Ada's currency symbol does not exist, instead, Ada is
-- | represented by `Coin`.
module Contract.Value
  ( module Value
  , module TokenName
  , scriptCurrencySymbol
  ) where

import Prelude
import Contract.Monad (Contract, wrapContract)
import Data.Maybe (Maybe)
import Scripts (scriptCurrencySymbol) as Scripts
import Types.Scripts (MintingPolicy)
import Types.TokenName
  ( TokenName
  , adaToken
  , getTokenName
  , mkTokenName
  , mkTokenNames
  ) as TokenName
import Types.Value
  ( Coin(Coin)
  , CurrencySymbol
  , NonAdaAsset(NonAdaAsset)
  , Value(Value)
  , coinToValue
  , currencyMPSHash
  , eq
  , filterNonAda
  , geq
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
  , numTokenNames
  , unionWith
  , valueOf
  , valueToCoin
  , valueToCoin'
  ) as Value

scriptCurrencySymbol
  :: forall (r :: Row Type)
   . MintingPolicy
  -> Contract r (Maybe Value.CurrencySymbol)
scriptCurrencySymbol = wrapContract <<< Scripts.scriptCurrencySymbol
