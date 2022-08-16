-- | A module for `QueryM` queries related to utxos.
module QueryM.Utxos
  ( filterLockedUtxos
  , getUtxo
  , getWalletBalance
  , utxosAt
  , getWalletCollateral
  ) where

import Prelude

import Address (addressToOgmiosAddress)
import Affjax.RequestBody (document)
import Cardano.Types.Transaction (TransactionOutput, UtxoM(UtxoM), Utxos)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.Value (Value)
import Contract.Prelude (Maybe(..))
import Control.Monad.Reader (withReaderT)
import Control.Monad.Reader.Trans (ReaderT, asks)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Bitraversable (bisequence)
import Data.Foldable (fold, foldr)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap, over)
import Data.Traversable (for, for_, sequence, traverse)
import Data.Tuple.Nested (type (/\))
import Data.UInt as UInt
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Helpers as Helpers
import QueryM (QueryM, callCip30Wallet, getWalletAddresses, mkOgmiosRequest)
import QueryM.Ogmios as Ogmios
import Serialization.Address (Address)
import TxOutput (ogmiosTxOutToTransactionOutput, txOutRefToTransactionInput)
import Types.Transaction (TransactionInput)
import Types.UsedTxOuts (UsedTxOuts, isTxOutRefUsed)
import Wallet (Wallet(Gero, Nami, Flint, Eternl, KeyWallet))

--------------------------------------------------------------------------------
-- UtxosAt
--------------------------------------------------------------------------------

-- If required, we can change to Either with more granular error handling.
-- | Gets utxos at an (internal) `Address` in terms of (internal) `Cardano.Transaction.Types`.
-- | Results may vary depending on `Wallet` type.
utxosAt
  :: Address
  -> QueryM (Maybe UtxoM)
utxosAt address =
  mkUtxoQuery
    <<< mkOgmiosRequest Ogmios.queryUtxosAtCall _.utxo
    $ addressToOgmiosAddress address

-- | Queries for UTxO given a transaction input.
getUtxo
  :: TransactionInput -> QueryM (Maybe TransactionOutput)
getUtxo ref =
  mkUtxoQuery
    (mkOgmiosRequest Ogmios.queryUtxoCall _.utxo ref) <#>
    (_ >>= unwrap >>> Map.lookup ref)

mkUtxoQuery :: QueryM Ogmios.UtxoQR -> QueryM (Maybe UtxoM)
mkUtxoQuery query = asks (_.runtime >>> _.wallet) >>= maybe allUtxosAt
  utxosAtByWallet
  where
  -- Add more wallet types here:
  utxosAtByWallet :: Wallet -> QueryM (Maybe UtxoM)
  utxosAtByWallet = case _ of
    Nami _ -> cip30UtxosAt
    Gero _ -> cip30UtxosAt
    Flint _ -> cip30UtxosAt
    Eternl _ -> cip30UtxosAt
    KeyWallet _ -> allUtxosAt

  -- Gets all utxos at an (internal) Address in terms of (internal)
  -- Cardano.Transaction.Types.
  allUtxosAt :: QueryM (Maybe UtxoM)
  allUtxosAt = convertUtxos <$> query
    where
    convertUtxos :: Ogmios.UtxoQR -> Maybe UtxoM
    convertUtxos (Ogmios.UtxoQR utxoQueryResult) =
      let
        out'
          :: Array
               ( Maybe TransactionInput /\ Maybe
                   TransactionOutput
               )
        out' = Map.toUnfoldable utxoQueryResult
          <#> bimap
            txOutRefToTransactionInput
            ogmiosTxOutToTransactionOutput

        out
          :: Maybe
               ( Array
                   ( TransactionInput /\
                       TransactionOutput
                   )
               )
        out = out' <#> bisequence # sequence
      in
        wrap <<< Map.fromFoldable <$> out

  cip30UtxosAt :: QueryM (Maybe UtxoM)
  cip30UtxosAt = getWalletCollateral >>= maybe
    (liftEffect $ throw "CIP-30 wallet missing collateral")
    \collateralUtxos ->
      allUtxosAt <#> \utxos' ->
        foldr
          ( \collateralUtxo utxoAcc ->
              over UtxoM (Map.delete (unwrap collateralUtxo).input) <$> utxoAcc
          )
          utxos'
          collateralUtxos

--------------------------------------------------------------------------------
-- Used Utxos helpers
--------------------------------------------------------------------------------

filterLockedUtxos :: Utxos -> QueryM Utxos
filterLockedUtxos utxos =
  withTxRefsCache $
    flip Helpers.filterMapWithKeyM utxos
      (\k _ -> not <$> isTxOutRefUsed (unwrap k))

withTxRefsCache
  :: forall (m :: Type -> Type) (a :: Type)
   . ReaderT UsedTxOuts Aff a
  -> QueryM a
withTxRefsCache = wrap <<< withReaderT (_.runtime >>> _.usedTxOuts)

getWalletBalance
  :: QueryM (Maybe Value)
getWalletBalance = do
  asks (_.runtime >>> _.wallet) >>= map join <<< traverse case _ of
    Nami wallet -> liftAff $ wallet.getBalance wallet.connection
    Gero wallet -> liftAff $ wallet.getBalance wallet.connection
    Eternl wallet -> liftAff $ wallet.getBalance wallet.connection
    Flint wallet -> liftAff $ wallet.getBalance wallet.connection
    KeyWallet _ -> do
      -- Implement via `utxosAt`
      mbAddresses <- getWalletAddresses

      map join $ for mbAddresses \addresses ->
        (map fold <<< sequence) <$> for addresses \address ->
          utxosAt address <#> map
            -- Combine `Value`s
            (fold <<< map _.amount <<< map unwrap <<< Map.values <<< unwrap)

getWalletCollateral :: QueryM (Maybe (Array TransactionUnspentOutput))
getWalletCollateral = do
  mbCollateralUTxOs <- asks (_.runtime >>> _.wallet) >>= maybe (pure Nothing)
    case _ of
      Nami nami -> liftAff $ callCip30Wallet nami _.getCollateral
      Gero gero -> liftAff $ callCip30Wallet gero _.getCollateral
      Flint flint -> liftAff $ callCip30Wallet flint _.getCollateral
      Eternl eternl -> liftAff $ callCip30Wallet eternl _.getCollateral
      KeyWallet kw -> do
        networkId <- asks $ _.config >>> _.networkId
        addr <- liftAff $ (unwrap kw).address networkId
        utxos <- utxosAt addr <#> map unwrap >>> fromMaybe Map.empty
          >>= filterLockedUtxos
        pure $ Array.singleton <$> (unwrap kw).selectCollateral utxos
  for_ mbCollateralUTxOs \collateralUTxOs -> do
    pparams <- asks $ _.runtime >>> _.pparams
    let
      tooManyCollateralUTxOs =
        fromMaybe false do
          maxCollateralInputs <- (unwrap pparams).maxCollateralInputs
          pure $ UInt.fromInt (Array.length collateralUTxOs) >
            maxCollateralInputs
    when tooManyCollateralUTxOs do
      liftEffect $ throw tooManyCollateralUTxOsError
  pure mbCollateralUTxOs
  where
  tooManyCollateralUTxOsError =
    "Wallet returned too many UTxOs as collateral. This is likely a bug in \
    \the wallet."
