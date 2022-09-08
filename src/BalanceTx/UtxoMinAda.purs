module BalanceTx.UtxoMinAda
  ( adaOnlyUtxoMinAdaValue
  , utxoMinAdaValue
  ) where

import Prelude

import BalanceTx.FakeOutput (fakeOutputWithValue)
import Cardano.Types.Transaction (TransactionOutput)
import Cardano.Types.Value (Coin, lovelaceValueOf)
import Data.BigInt (BigInt)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Partial.Unsafe (unsafePartial)
import Serialization (convertTxOutput)
import Cardano.Types.Value (Coin(Coin), lovelaceValueOf)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Reader.Class (asks)
import Data.BigInt (BigInt)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (wrap, unwrap)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Partial.Unsafe (unsafePartial)
import QueryM.Ogmios (CoinsPerUtxoUnit(CoinsPerUtxoWord, CoinsPerUtxoByte))
import Serialization (convertTxOutput)
import Serialization.Address (addressFromBech32) as Csl
import Serialization.Types (DataCost)
import Serialization.Types (TransactionOutput) as Csl
import Types.BigNum (BigNum)
import Types.BigNum (fromBigInt, maxValue, toBigInt, toBigIntUnsafe) as BigNum

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
  cslTxOutput <- liftEffect $ convertTxOutput txOutput
  pure $ minAdaForOutput maybeFfiHelper cslTxOutput dataCost
    -- useful spy: BigNum.toBigInt >>= (pure <<< spy "utxoMinAdaValue")
    >>= BigNum.toBigInt

adaOnlyUtxoMinAdaValue :: CoinsPerUtxoUnit -> Effect BigInt
adaOnlyUtxoMinAdaValue coinsPerUtxoUnit =
  map (unsafePartial fromJust) <<< utxoMinAdaValue coinsPerUtxoUnit <<<
    fakeOutputWithValue
    $ lovelaceValueOf (BigNum.toBigIntUnsafe BigNum.maxValue)
