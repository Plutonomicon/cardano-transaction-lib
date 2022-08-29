module BalanceTx.UtxoMinAda
  ( adaOnlyUtxoMinAdaValue
  , utxoMinAdaValue
  ) where

import Prelude

import Cardano.Types.Transaction (TransactionOutput)
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
import QueryM (QueryM)
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

utxoMinAdaValue :: TransactionOutput -> QueryM (Maybe BigInt)
utxoMinAdaValue txOutput = do
  coinsPerUtxoUnit <- asks (_.runtime >>> _.pparams) <#> unwrap >>>
    _.coinsPerUtxoUnit
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

adaOnlyUtxoMinAdaValue :: QueryM BigInt
adaOnlyUtxoMinAdaValue =
  -- useful spy: unsafePartial fromJust <<< spy "adaOnlyUtxoMinAdaValue"
  map (unsafePartial fromJust) <<< utxoMinAdaValue $ wrap
    { -- this fake address is taken from CSL
      -- https://github.com/Emurgo/cardano-serialization-lib/blob/a58bfa583297705ffc0fb03923cecef3452a6aee/rust/src/utils.rs#L1146
      address: unsafePartial fromJust $ Csl.addressFromBech32
        "addr_test1qpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ewvxwdrt70qlcpe\
        \eagscasafhffqsxy36t90ldv06wqrk2qum8x5w"
    , amount: lovelaceValueOf $ BigNum.toBigIntUnsafe BigNum.maxValue
    , dataHash: Nothing
    }
