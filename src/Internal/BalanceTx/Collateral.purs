module Ctl.Internal.BalanceTx.Collateral
  ( addTxCollateral
  , addTxCollateralReturn
  , module X
  ) where

import Prelude

import Cardano.Types
  ( BigNum
  , Coin
  , MultiAsset
  , Transaction
  , TransactionOutput
  , TransactionUnspentOutput
  , _body
  , _collateral
  , _collateralReturn
  , _totalCollateral
  )
import Cardano.Types.Address (Address)
import Cardano.Types.BigNum (add, max, maxValue, sub, zero) as BigNum
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.Value (getMultiAsset, mkValue, valueToCoin) as Value
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (except)
import Ctl.Internal.BalanceTx.Collateral.Select (minRequiredCollateral)
import Ctl.Internal.BalanceTx.Collateral.Select (minRequiredCollateral) as X
import Ctl.Internal.BalanceTx.Error
  ( BalanceTxError(NumericOverflowError, CollateralReturnError)
  )
import Ctl.Internal.BalanceTx.Types (BalanceTxM, askCoinsPerUtxoUnit)
import Ctl.Internal.BalanceTx.UtxoMinAda (utxoMinAdaValue)
import Data.Either (Either(Left, Right))
import Data.Foldable (foldl)
import Data.Lens ((.~))
import Data.Lens.Setter ((?~))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)

addTxCollateral :: Array TransactionUnspentOutput -> Transaction -> Transaction
addTxCollateral collateral transaction =
  transaction # _body <<< _collateral .~
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
addTxCollateralReturn collateral transaction ownAddress = do
  let maybeAdd acc n = BigNum.add (unwrap $ adaValue n) =<< acc
  collAdaValue <- throwOnOverflow $ foldl maybeAdd (Just BigNum.zero) collateral
  collMultiAsset <- throwOnOverflow $ MultiAsset.sum $ nonAdaAsset <$>
    collateral
  case
    collAdaValue <= unwrap minRequiredCollateral && collMultiAsset ==
      MultiAsset.empty
    of
    true ->
      pure transaction
    false ->
      setTxCollateralReturn collAdaValue collMultiAsset
  where
  setTxCollateralReturn
    :: BigNum
    -> MultiAsset
    -> BalanceTxM Transaction
  setTxCollateralReturn collAdaValue collMultiAsset = do
    let
      maxBigNumAdaValue :: Coin
      maxBigNumAdaValue = wrap BigNum.maxValue

      collReturnOutputRec =
        { address: ownAddress
        , amount: Value.mkValue maxBigNumAdaValue collMultiAsset
        , datum: Nothing
        , scriptRef: Nothing
        }

    coinsPerUtxoUnit <- askCoinsPerUtxoUnit

    -- Calculate the required min ada value for the collateral return output:
    minAdaValue <- do
      let returnAsTxOut = wrap collReturnOutputRec
      pure $ utxoMinAdaValue coinsPerUtxoUnit returnAsTxOut
    -- Determine the actual ada value of the collateral return output:
    collReturnAda <- throwOnOverflow do
      remaining <- BigNum.sub collAdaValue (unwrap minRequiredCollateral)
      pure $ BigNum.max remaining minAdaValue
    let
      -- Build the final collateral return output:
      collReturnOutput :: TransactionOutput
      collReturnOutput = wrap $
        collReturnOutputRec
          { amount = Value.mkValue (wrap collReturnAda) collMultiAsset }

    totalCollateral <- throwOnOverflow $ BigNum.sub collAdaValue collReturnAda

    except $
      case totalCollateral > BigNum.zero of
        true ->
          -- Set collateral return and total collateral:
          Right $
            transaction # _body <<< _collateralReturn ?~ collReturnOutput
              # _body <<< _totalCollateral ?~ wrap totalCollateral
        false -> Left CollateralReturnError

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

adaValue :: TransactionUnspentOutput -> Coin
adaValue =
  Value.valueToCoin <<< _.amount <<< unwrap <<< _.output <<< unwrap

nonAdaAsset :: TransactionUnspentOutput -> MultiAsset
nonAdaAsset =
  Value.getMultiAsset <<< _.amount <<< unwrap <<< _.output <<< unwrap

throwOnOverflow :: forall a. Maybe a -> BalanceTxM a
throwOnOverflow = case _ of
  Nothing -> throwError (NumericOverflowError Nothing)
  Just a -> pure a
