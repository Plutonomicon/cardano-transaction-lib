module Types.Redeemer
  ( unitRedeemer
  ) where

import Types.PlutusData (PlutusData(..))
import Types.RedeemerTag (RedeemerTag)
import Types.Transaction (Redeemer(..))
import Data.BigInt as BigInt

-- FIX ME (create an issue unless someone notices simple solution to this in PR
-- review)
unitRedeemer :: RedeemerTag -> Redeemer
unitRedeemer tag = Redeemer
  { tag: tag
  , index: BigInt.fromInt 0
  , data: List []
  , ex_units: { mem: BigInt.fromInt 0, steps: BigInt.fromInt 0 }
  }
