module BalanceTx.UtxoMinAda
  ( adaOnlyUtxoMinAdaValue
  , utxoMinAdaValue
  ) where

import Prelude

import BalanceTx.Helpers (fakeOutputWithValue)
import Cardano.Types.Transaction (TransactionOutput)
import Cardano.Types.Value (lovelaceValueOf)
import Control.Monad.Reader.Class (asks)
import Data.BigInt (BigInt)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Partial.Unsafe (unsafePartial)
import QueryM (QueryM)
import Serialization (convertTxOutput)
import Serialization.Types (TransactionOutput) as Csl
import Types.BigNum (BigNum)
import Types.BigNum (fromBigInt, maxValue, toBigInt, toBigIntUnsafe) as BigNum

foreign import minAdaForOutput
  :: MaybeFfiHelper -> Csl.TransactionOutput -> BigNum -> Maybe BigNum

utxoMinAdaValue :: TransactionOutput -> QueryM (Maybe BigInt)
utxoMinAdaValue txOutput = do
  coinsPerUtxoByte <- asks (_.runtime >>> _.pparams) <#> unwrap >>>
    _.coinsPerUtxoByte
  cslTxOutput <- liftEffect $ convertTxOutput txOutput
  pure $ BigNum.fromBigInt (unwrap coinsPerUtxoByte)
    >>= minAdaForOutput maybeFfiHelper cslTxOutput
    -- useful spy: BigNum.toBigInt >>= (pure <<< spy "utxoMinAdaValue")
    >>= BigNum.toBigInt

adaOnlyUtxoMinAdaValue :: QueryM BigInt
adaOnlyUtxoMinAdaValue =
  map (unsafePartial fromJust) <<< utxoMinAdaValue <<< fakeOutputWithValue
    $ lovelaceValueOf (BigNum.toBigIntUnsafe BigNum.maxValue)
