module BalanceTx.Collateral
  ( addTxCollateral
  , addTxCollateralReturn
  , module X
  ) where

import Prelude

import BalanceTx.Collateral.Select (minRequiredCollateral)
import BalanceTx.Collateral.Select (minRequiredCollateral) as X
import BalanceTx.Error
  ( BalanceTxError(CollateralReturnError, CollateralReturnMinAdaValueCalcError)
  )
import BalanceTx.Types
  ( BalanceTxM
  )
import BalanceTx.UtxoMinAda (utxoMinAdaValue)
import Cardano.Types.Transaction
  ( Transaction
  , TransactionOutput
  , _body
  , _collateral
  , _collateralReturn
  , _totalCollateral
  )
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.Value (Coin, NonAdaAsset)
import Cardano.Types.Value (getNonAdaAsset, mkValue, valueToCoin') as Value
import Control.Monad.Except.Trans (ExceptT(ExceptT), except)
import Control.Monad.Reader.Class (asks)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldl, foldMap)
import Data.Lens.Setter ((?~))
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (wrap, unwrap)
import Data.Ord.Max (Max(Max))
import Effect.Class (liftEffect)
import Serialization.Address (Address)
import Types.BigNum (maxValue, toBigIntUnsafe) as BigNum
import Types.OutputDatum (OutputDatum(NoOutputDatum))

addTxCollateral :: Array TransactionUnspentOutput -> Transaction -> Transaction
addTxCollateral collateral transaction =
  transaction # _body <<< _collateral ?~
    map (_.input <<< unwrap) collateral

--------------------------------------------------------------------------------
-- Collateral Return, Total Collateral
--------------------------------------------------------------------------------

-- | Sets `collateral return` and `total collateral` fields of the transaction.
-- | In the special case with an Ada-only collateral that is less than or equal
-- | to `minRequiredCollateral`, returns unmodified transaction (see NOTE).
-- |
-- | NOTE: Collateral cannot be less than `minRequiredCollateral` when
-- | selected using `selectCollateral` function in this module.
addTxCollateralReturn
  :: Array TransactionUnspentOutput
  -> Transaction
  -> Address
  -> BalanceTxM Transaction
addTxCollateralReturn collateral transaction ownAddress =
  let
    collAdaValue :: BigInt
    collAdaValue = foldl adaValue' zero collateral

    collNonAdaAsset :: NonAdaAsset
    collNonAdaAsset = foldMap nonAdaAsset collateral
  in
    case collAdaValue <= minRequiredCollateral && collNonAdaAsset == mempty of
      true ->
        pure transaction
      false ->
        setTxCollateralReturn collAdaValue collNonAdaAsset
  where
  setTxCollateralReturn
    :: BigInt
    -> NonAdaAsset
    -> BalanceTxM Transaction
  setTxCollateralReturn collAdaValue collNonAdaAsset = do
    let
      maxBigNumAdaValue :: Coin
      maxBigNumAdaValue = wrap (BigNum.toBigIntUnsafe BigNum.maxValue)

      collReturnOutputRec =
        { address: ownAddress
        , amount: Value.mkValue maxBigNumAdaValue collNonAdaAsset
        , datum: NoOutputDatum
        , scriptRef: Nothing
        }

    coinsPerUtxoByte <-
      asks (_.runtime >>> _.pparams) <#> unwrap >>> _.coinsPerUtxoByte

    -- Calculate the required min ada value for the collateral return output:
    minAdaValue <-
      ExceptT $
        liftEffect (utxoMinAdaValue coinsPerUtxoByte (wrap collReturnOutputRec))
          <#> note CollateralReturnMinAdaValueCalcError

    let
      -- Determine the actual ada value of the collateral return output:
      collReturnAda :: BigInt
      collReturnAda = unwrap $
        Max (collAdaValue - minRequiredCollateral) <> Max minAdaValue

      -- Build the final collateral return output:
      collReturnOutput :: TransactionOutput
      collReturnOutput = wrap $
        collReturnOutputRec
          { amount = Value.mkValue (wrap collReturnAda) collNonAdaAsset }

      totalCollateral :: BigInt
      totalCollateral = collAdaValue - collReturnAda

    except $
      case totalCollateral > zero of
        true ->
          -- Set collateral return and total collateral:
          Right $
            transaction # _body <<< _collateralReturn ?~ collReturnOutput
              # _body <<< _totalCollateral ?~ wrap totalCollateral
        false ->
          Left $ CollateralReturnError
            "Negative totalCollateral after covering min-utxo-ada requirement."

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

adaValue :: TransactionUnspentOutput -> BigInt
adaValue =
  Value.valueToCoin' <<< _.amount <<< unwrap <<< _.output <<< unwrap

adaValue' :: BigInt -> TransactionUnspentOutput -> BigInt
adaValue' init = add init <<< adaValue

nonAdaAsset :: TransactionUnspentOutput -> NonAdaAsset
nonAdaAsset =
  Value.getNonAdaAsset <<< _.amount <<< unwrap <<< _.output <<< unwrap
