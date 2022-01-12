module Ada
  ( adaSymbol
  , adaToken
  ) where

import Types.Transaction (CurrencySymbol(..), TokenName(..))

adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol ""

adaToken :: TokenName
adaToken = TokenName ""