module Ctl.Internal.BalanceTx.UtxoMinAda
  ( adaOnlyUtxoMinAdaValue
  , utxoMinAdaValue
  ) where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.BalanceTx.FakeOutput (fakeOutputWithValue)
import Ctl.Internal.Cardano.Types.Transaction (TransactionOutput)
import Ctl.Internal.Cardano.Types.Value (Coin(Coin), lovelaceValueOf)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Serialization (convertTxOutput)
import Ctl.Internal.Serialization.Types (DataCost)
import Ctl.Internal.Serialization.Types (TransactionOutput) as Csl
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.BigNum
  ( fromBigInt
  , maxValue
  , toBigInt
  ) as BigNum
import Ctl.Internal.Types.ProtocolParameters
  ( CoinsPerUtxoUnit(CoinsPerUtxoWord, CoinsPerUtxoByte)
  )
import Data.Maybe (Maybe, fromJust)
import Effect (Effect)
import Effect.Exception (error)
import JS.BigInt (BigInt)
import Partial.Unsafe (unsafePartial)

foreign import minAdaForOutput
  :: MaybeFfiHelper -> Csl.TransactionOutput -> DataCost -> Maybe BigNum

foreign import newCoinsPerWord :: BigNum -> DataCost

foreign import newCoinsPerByte :: BigNum -> DataCost

utxoMinAdaValue
  :: CoinsPerUtxoUnit -> TransactionOutput -> Effect (Maybe BigInt)
utxoMinAdaValue coinsPerUtxoUnit txOutput = do
  cslTxOutput <- convertTxOutput txOutput
  dataCost <- case coinsPerUtxoUnit of
    CoinsPerUtxoByte (Coin n) -> do
      newCoinsPerByte <$> liftMaybe (error "Failed to convert CoinsPerUtxoByte")
        (BigNum.fromBigInt n)
    CoinsPerUtxoWord (Coin n) -> do
      newCoinsPerWord <$> liftMaybe (error "Failed to convert CoinsPerUtxoWord")
        (BigNum.fromBigInt n)
  pure $ minAdaForOutput maybeFfiHelper cslTxOutput dataCost
    -- useful spy: BigNum.toBigInt >>= (pure <<< spy "utxoMinAdaValue")
    <#> BigNum.toBigInt

adaOnlyUtxoMinAdaValue :: CoinsPerUtxoUnit -> Effect BigInt
adaOnlyUtxoMinAdaValue coinsPerUtxoUnit =
  map (unsafePartial fromJust) <<< utxoMinAdaValue coinsPerUtxoUnit <<<
    fakeOutputWithValue
    $ lovelaceValueOf (BigNum.toBigInt BigNum.maxValue)
