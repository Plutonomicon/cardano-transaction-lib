-- | A module that defines tokens in Cardano and helpers.
module Contract.Value
  ( module CurrencySymbol
  , module TokenName
  , module Value
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
import Plutus.Types.CurrencySymbol
  ( CurrencySymbol
  , adaSymbol
  , currencyMPSHash
  , getCurrencySymbol
  , mkCurrencySymbol
  , mpsSymbol
  , scriptHashAsCurrencySymbol
  ) as CurrencySymbol
import Plutus.Types.Value
  ( Value
  , getValue
  , singleton
  , singleton'
  , valueOf
  , lovelaceValueOf
  , scale
  , symbols
  , isZero
  , negation
  , split
  , unionWith
  , flattenValue
  , flattenNonAdaAssets
  , geq
  , gt
  , leq
  , lt
  ) as Value

scriptCurrencySymbol
  :: forall (r :: Row Type)
   . MintingPolicy
  -> Contract r (Maybe CurrencySymbol.CurrencySymbol)
scriptCurrencySymbol = wrapContract <<< Scripts.scriptCurrencySymbol
