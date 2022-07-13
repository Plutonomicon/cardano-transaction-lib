-- | A module for `QueryM` queries related to utxos.
module QueryM.Utxos
  ( filterLockedUtxos
  , getUtxo
  , getWalletBalance
  , utxosAt
  ) where

import Prelude

import Address (addressToOgmiosAddress)
import Cardano.Types.Transaction (TransactionOutput, UtxoM(UtxoM), Utxos)
import Cardano.Types.Value (Value)
import Control.Monad.Logger.Trans (LoggerT)
import Control.Monad.Reader (withReaderT)
import Control.Monad.Reader.Trans (ReaderT, asks)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bisequence)
import Data.Foldable (fold)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap, wrap, over)
import Data.Traversable (for, sequence, traverse)
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Helpers as Helpers
import Prim.TypeError (class Warn, Text)
import QueryM (QueryM, getWalletAddress, getWalletCollateral, mkOgmiosRequest)
import QueryM.Ogmios as Ogmios
import Serialization.Address (Address)
import TxOutput (ogmiosTxOutToTransactionOutput, txOutRefToTransactionInput)
import Types.Transaction (TransactionInput)
import Types.UsedTxOuts (UsedTxOuts, isTxOutRefUsed)
import Wallet (Wallet(Gero, Nami, KeyWallet))

--------------------------------------------------------------------------------
-- UtxosAt
--------------------------------------------------------------------------------

-- If required, we can change to Either with more granular error handling.
-- | Gets utxos at an (internal) `Address` in terms of (internal) `Cardano.Transaction.Types`.
-- | Results may vary depending on `Wallet` type.
utxosAt
  :: Warn
       ( Text
           "`utxosAt`: Querying for UTxOs by address is deprecated. See https://github.com/Plutonomicon/cardano-transaction-lib/issues/536."
       )
  => Address
  -> QueryM (Maybe UtxoM)
utxosAt = mkUtxoQuery
  <<< mkOgmiosRequest Ogmios.queryUtxosAtCall _.utxo
  <<< addressToOgmiosAddress

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
    \collateral' -> do
      let collateral = unwrap collateral'
      utxos' <- allUtxosAt
      pure (over UtxoM (Map.delete collateral.input) <$> utxos')

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
   . ReaderT UsedTxOuts (LoggerT Aff) a
  -> QueryM a
withTxRefsCache f = withReaderT (_.runtime >>> _.usedTxOuts) f

getWalletBalance
  :: QueryM (Maybe Value)
getWalletBalance = do
  asks (_.runtime >>> _.wallet) >>= map join <<< traverse case _ of
    Nami wallet -> liftAff $ wallet.getBalance wallet.connection
    Gero wallet -> liftAff $ wallet.getBalance wallet.connection
    KeyWallet _ -> do
      -- Implement via `utxosAt`
      mbAddress <- getWalletAddress
      map join $ for mbAddress \address -> do
        utxosAt address <#> map
          -- Combine `Value`s
          (fold <<< map _.amount <<< map unwrap <<< Map.values <<< unwrap)
