module BalanceTx.UtxoMinAda (utxoMinAdaValue) where

import Prelude

import Cardano.Types.Transaction (TransactionOutput)
import Control.Monad.Reader.Class
import Data.BigInt (BigInt)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import QueryM (QueryM)
import Serialization (convertTxOutput)
import Serialization.Types (TransactionOutput) as Csl
import Types.BigNum (BigNum)
import Types.BigNum (fromBigInt, toBigInt) as BigNum

foreign import minAdaForOutput
  :: MaybeFfiHelper -> Csl.TransactionOutput -> BigNum -> Maybe BigNum

utxoMinAdaValue :: TransactionOutput -> QueryM (Maybe BigInt)
utxoMinAdaValue txOutput = do
  coinsPerUtxoByte <- asks _.pparams <#> unwrap >>> _.coinsPerUtxoByte
  cslTxOutput <- liftEffect $ convertTxOutput txOutput
  pure $ BigNum.fromBigInt (unwrap coinsPerUtxoByte)
    >>= minAdaForOutput maybeFfiHelper cslTxOutput
    >>= BigNum.toBigInt
