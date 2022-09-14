module CTL.Internal.BalanceTx.UtxoMinAda
  ( adaOnlyUtxoMinAdaValue
  , utxoMinAdaValue
  ) where

import Prelude

import CTL.Internal.BalanceTx.FakeOutput (fakeOutputWithValue)
import CTL.Internal.Cardano.Types.Transaction (TransactionOutput)
import CTL.Internal.Cardano.Types.Value (Coin(Coin), lovelaceValueOf)
import Control.Monad.Error.Class (liftMaybe)
import Data.BigInt (BigInt)
import Data.Maybe (Maybe, fromJust)
import Effect (Effect)
import Effect.Exception (error)
import CTL.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Partial.Unsafe (unsafePartial)
import CTL.Internal.Serialization (convertTxOutput)
import CTL.Internal.QueryM.Ogmios (CoinsPerUtxoUnit(CoinsPerUtxoWord, CoinsPerUtxoByte))
import CTL.Internal.Serialization.Types (DataCost)
import CTL.Internal.Serialization.Types (TransactionOutput) as Csl
import CTL.Internal.Types.BigNum (BigNum)
import CTL.Internal.Types.BigNum (fromBigInt, maxValue, toBigInt, toBigIntUnsafe) as BigNum

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
    >>= BigNum.toBigInt

adaOnlyUtxoMinAdaValue :: CoinsPerUtxoUnit -> Effect BigInt
adaOnlyUtxoMinAdaValue coinsPerUtxoUnit =
  map (unsafePartial fromJust) <<< utxoMinAdaValue coinsPerUtxoUnit <<<
    fakeOutputWithValue
    $ lovelaceValueOf (BigNum.toBigIntUnsafe BigNum.maxValue)
