module BalanceTx.UtxoMinAda
  ( adaOnlyUtxoMinAdaValue
  , utxoMinAdaValue
  ) where

import Prelude

import BalanceTx.Helpers (fakeOutputWithValue)
import Cardano.Types.Transaction (TransactionOutput)
import Cardano.Types.Value (Coin, lovelaceValueOf)
import Data.BigInt (BigInt)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Partial.Unsafe (unsafePartial)
import Serialization (convertTxOutput)
import Serialization.Types (TransactionOutput) as Csl
import Types.BigNum (BigNum)
import Types.BigNum (fromBigInt, maxValue, toBigInt, toBigIntUnsafe) as BigNum

foreign import minAdaForOutput
  :: MaybeFfiHelper -> Csl.TransactionOutput -> BigNum -> Maybe BigNum

utxoMinAdaValue :: Coin -> TransactionOutput -> Effect (Maybe BigInt)
utxoMinAdaValue coinsPerUtxoByte txOutput = do
  cslTxOutput <- convertTxOutput txOutput
  pure $ BigNum.fromBigInt (unwrap coinsPerUtxoByte)
    >>= minAdaForOutput maybeFfiHelper cslTxOutput
    -- useful spy: BigNum.toBigInt >>= (pure <<< spy "utxoMinAdaValue")
    >>= BigNum.toBigInt

adaOnlyUtxoMinAdaValue :: Coin -> Effect BigInt
adaOnlyUtxoMinAdaValue coinsPerUtxoByte =
  map (unsafePartial fromJust) <<< utxoMinAdaValue coinsPerUtxoByte <<<
    fakeOutputWithValue
    $ lovelaceValueOf (BigNum.toBigIntUnsafe BigNum.maxValue)
