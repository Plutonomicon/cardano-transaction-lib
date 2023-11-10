module Ctl.Internal.BalanceTx.Collateral
  ( addTxCollateral
  , addTxCollateralReturn
  , module X
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT(ExceptT), except)
import Ctl.Internal.BalanceTx.Collateral.Select (minRequiredCollateral)
import Ctl.Internal.BalanceTx.Collateral.Select (minRequiredCollateral) as X
import Ctl.Internal.BalanceTx.Error
  ( BalanceTxError(CollateralReturnError, CollateralReturnMinAdaValueCalcError)
  )
import Ctl.Internal.BalanceTx.Types (BalanceTxM, askCoinsPerUtxoUnit)
import Ctl.Internal.BalanceTx.UtxoMinAda (utxoMinAdaValue)
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction
  , TransactionOutput
  , _body
  , _collateral
  , _collateralReturn
  , _totalCollateral
  )
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import Ctl.Internal.Cardano.Types.Value (Coin, NonAdaAsset)
import Ctl.Internal.Cardano.Types.Value (getNonAdaAsset, mkValue, valueToCoin') as Value
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Types.BigNum (maxValue, toBigInt) as BigNum
import Ctl.Internal.Types.OutputDatum (OutputDatum(NoOutputDatum))
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldMap, foldl)
import Data.Lens.Setter ((?~))
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Ord.Max (Max(Max))
import Effect.Class (liftEffect)
import JS.BigInt (BigInt)

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
-- | selected using `selectCollateral` function
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
      maxBigNumAdaValue = wrap (BigNum.toBigInt BigNum.maxValue)

      collReturnOutputRec =
        { address: ownAddress
        , amount: Value.mkValue maxBigNumAdaValue collNonAdaAsset
        , datum: NoOutputDatum
        , scriptRef: Nothing
        }

    coinsPerUtxoUnit <- askCoinsPerUtxoUnit

    -- Calculate the required min ada value for the collateral return output:
    minAdaValue <- do
      let returnAsTxOut = wrap collReturnOutputRec
      ExceptT $
        liftEffect (utxoMinAdaValue coinsPerUtxoUnit returnAsTxOut)
          <#> note
            ( CollateralReturnMinAdaValueCalcError coinsPerUtxoUnit
                returnAsTxOut
            )

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
        false -> Left CollateralReturnError

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
