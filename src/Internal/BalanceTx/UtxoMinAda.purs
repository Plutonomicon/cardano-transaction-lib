module Ctl.Internal.BalanceTx.UtxoMinAda
  ( adaOnlyUtxoMinAdaValue
  , utxoMinAdaValue
  ) where

import Prelude

import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum (maxValue) as BigNum
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.TransactionOutput as TransactionOutput
import Ctl.Internal.BalanceTx.FakeOutput (fakeOutputWithValue)
import Ctl.Internal.Cardano.Types.Value (Coin(Coin), lovelaceValueOf)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Serialization.Types (DataCost)
import Ctl.Internal.Serialization.Types (TransactionOutput) as Csl
import Ctl.Internal.Types.ProtocolParameters
  ( CoinsPerUtxoUnit(CoinsPerUtxoWord, CoinsPerUtxoByte)
  )
import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)

foreign import minAdaForOutput
  :: MaybeFfiHelper -> Csl.TransactionOutput -> DataCost -> Maybe BigNum

foreign import newCoinsPerWord :: BigNum -> DataCost

foreign import newCoinsPerByte :: BigNum -> DataCost

utxoMinAdaValue
  :: CoinsPerUtxoUnit -> TransactionOutput -> Maybe BigNum
utxoMinAdaValue coinsPerUtxoUnit txOutput =
  let
    cslTxOutput = TransactionOutput.toCsl txOutput
    dataCost = case coinsPerUtxoUnit of
      CoinsPerUtxoByte (Coin n) -> newCoinsPerByte n
      CoinsPerUtxoWord (Coin n) -> newCoinsPerWord n
  in
    minAdaForOutput maybeFfiHelper cslTxOutput dataCost

-- useful spy: BigNum.toBigInt >>= (pure <<< spy "utxoMinAdaValue")

adaOnlyUtxoMinAdaValue :: CoinsPerUtxoUnit -> BigNum
adaOnlyUtxoMinAdaValue coinsPerUtxoUnit =
  unsafePartial fromJust <<< utxoMinAdaValue coinsPerUtxoUnit <<<
    fakeOutputWithValue
    $ lovelaceValueOf BigNum.maxValue
