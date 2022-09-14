-- | A module that defines tokens in Cardano and helpers.
module CTL.Contract.Value
  ( module CurrencySymbol
  , module Scripts
  , module TokenName
  , module Value
  ) where

import CTL.Internal.Plutus.Types.CurrencySymbol
  ( CurrencySymbol
  , adaSymbol
  , currencyMPSHash
  , getCurrencySymbol
  , mkCurrencySymbol
  , mpsSymbol
  , scriptHashAsCurrencySymbol
  ) as CurrencySymbol
import CTL.Internal.Plutus.Types.Value
  ( Coin(Coin)
  , Value
  , coinToValue
  , flattenNonAdaAssets
  , flattenValue
  , geq
  , getLovelace
  , getValue
  , gt
  , isCoinZero
  , isZero
  , leq
  , lovelaceValueOf
  , lt
  , negation
  , scale
  , singleton
  , singleton'
  , split
  , symbols
  , unionWith
  , valueOf
  , valueToCoin
  , valueToCoin'
  ) as Value
import CTL.Internal.Scripts (scriptCurrencySymbol) as Scripts
import CTL.Internal.Types.TokenName
  ( TokenName
  , adaToken
  , getTokenName
  , mkTokenName
  , mkTokenNames
  ) as TokenName
