module Ctl.Internal.BalanceTx.UtxoMinAda
  ( adaOnlyUtxoMinAdaValue
  , utxoMinAdaValue
  ) where

import Prelude

import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum (maxValue) as BigNum
import Cardano.Types.Coin (Coin)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.TransactionOutput as TransactionOutput
import Cardano.Types.Value (lovelaceValueOf)
import Ctl.Internal.BalanceTx.FakeOutput (fakeOutputWithValue)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Serialization.Types (DataCost)
import Ctl.Internal.Serialization.Types (TransactionOutput) as Csl
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)

foreign import minAdaForOutput
  :: MaybeFfiHelper -> Csl.TransactionOutput -> DataCost -> Maybe BigNum

foreign import newCoinsPerWord :: BigNum -> DataCost

foreign import newCoinsPerByte :: BigNum -> DataCost

utxoMinAdaValue
  :: Coin -> TransactionOutput -> Maybe BigNum
utxoMinAdaValue coinsPerUtxoByte txOutput =
  let
    cslTxOutput = TransactionOutput.toCsl txOutput
    dataCost = newCoinsPerByte (unwrap coinsPerUtxoByte)
  in
    minAdaForOutput maybeFfiHelper cslTxOutput dataCost

-- useful spy: BigNum.toBigInt >>= (pure <<< spy "utxoMinAdaValue")

adaOnlyUtxoMinAdaValue :: Coin -> BigNum
adaOnlyUtxoMinAdaValue coinsPerUtxoByte =
  unsafePartial fromJust <<< utxoMinAdaValue coinsPerUtxoByte <<<
    fakeOutputWithValue
    $ lovelaceValueOf BigNum.maxValue
