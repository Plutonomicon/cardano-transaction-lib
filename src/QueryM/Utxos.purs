-- | A module for `QueryM` queries related to utxos.
module QueryM.Utxos
  ( filterUnusedUtxos
  , utxosAt
  ) where

import Prelude

import Address (addressToOgmiosAddress)
import Cardano.Types.Transaction
  ( TransactionOutput
  , UtxoM(UtxoM)
  )
import Control.Monad.Logger.Trans (LoggerT)
import Control.Monad.Reader (withReaderT)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bisequence)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Helpers as Helpers
import QueryM (QueryM, mkOgmiosRequest)
import Serialization.Address (Address)
import Types.Transaction (TransactionInput)
import TxOutput (ogmiosTxOutToTransactionOutput, txOutRefToTransactionInput)
import Types.UsedTxOuts (UsedTxOuts, isTxOutRefUsed)
import QueryM.Ogmios as Ogmios

--------------------------------------------------------------------------------
-- UtxosAt
--------------------------------------------------------------------------------

-- If required, we can change to Either with more granular error handling.
-- | Gets utxos at an (internal) `Address` in terms of (internal) `Cardano.Transaction.Types`.
-- | Results may vary depending on `Wallet` type.
utxosAt :: Address -> QueryM (Maybe UtxoM)
utxosAt = addressToOgmiosAddress >>> getUtxos
  where
  utxosAt' :: Ogmios.OgmiosAddress -> QueryM Ogmios.UtxoQR
  utxosAt' addr' = mkOgmiosRequest Ogmios.queryUtxosAtCall _.utxo addr'

  getUtxos :: Ogmios.OgmiosAddress -> QueryM (Maybe UtxoM)
  getUtxos address = convertUtxos <$> utxosAt' address

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
      (wrap <<< Map.fromFoldable) <$> out

--------------------------------------------------------------------------------
-- Used Utxos helpers
--------------------------------------------------------------------------------

filterUnusedUtxos :: UtxoM -> QueryM UtxoM
filterUnusedUtxos (UtxoM utxos) = withTxRefsCache $
  UtxoM <$> Helpers.filterMapWithKeyM (\k _ -> isTxOutRefUsed (unwrap k)) utxos

withTxRefsCache
  :: forall (m :: Type -> Type) (a :: Type)
   . ReaderT UsedTxOuts (LoggerT Aff) a
  -> QueryM a
withTxRefsCache f = withReaderT (_.usedTxOuts) f
