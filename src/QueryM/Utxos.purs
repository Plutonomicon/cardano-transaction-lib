-- | A module for `QueryM` queries related to utxos.
module QueryM.Utxos
  ( filterUnusedUtxos
  , utxosAt
  ) where

import Prelude

import Address (addressToOgmiosAddress)
import Control.Monad.Logger.Trans (LoggerT)
import Control.Monad.Reader (withReaderT)
import Control.Monad.Reader.Trans (ReaderT, asks)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bisequence)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Helpers as Helpers
import QueryM (QueryM, getWalletCollateral, mkOgmiosRequest)
import Serialization.Address (Address)
import Types.Transaction (UtxoM(UtxoM))
import Types.Transaction as Transaction
import TxOutput (ogmiosTxOutToTransactionOutput, txOutRefToTransactionInput)
import Types.UsedTxOuts (UsedTxOuts, isTxOutRefUsed)
import Wallet (Wallet(Nami))
import QueryM.Ogmios as Ogmios

--------------------------------------------------------------------------------
-- UtxosAt
--------------------------------------------------------------------------------

-- If required, we can change to Either with more granular error handling.
-- | Gets utxos at an (internal) `Address` in terms of (internal) `Transaction.Types`.
-- | Results may vary depending on `Wallet` type.
utxosAt :: Address -> QueryM (Maybe Transaction.UtxoM)
utxosAt addr = asks _.wallet >>= maybe (allUtxosAt addr) (utxosAtByWallet addr)
  where
  -- Add more wallet types here:
  utxosAtByWallet
    :: Address -> Wallet -> QueryM (Maybe Transaction.UtxoM)
  utxosAtByWallet address (Nami _) = namiUtxosAt address
  -- Unreachable but helps build when we add wallets, most of them shouldn't
  -- require any specific behaviour.
  utxosAtByWallet address _ = allUtxosAt address

  -- Gets all utxos at an (internal) Address in terms of (internal)
  -- Transaction.Types.
  allUtxosAt :: Address -> QueryM (Maybe Transaction.UtxoM)
  allUtxosAt = addressToOgmiosAddress >>> getUtxos
    where
    utxosAt' :: Ogmios.OgmiosAddress -> QueryM Ogmios.UtxoQR
    utxosAt' addr' = mkOgmiosRequest Ogmios.queryUtxosAtCall _.utxo addr'

    getUtxos :: Ogmios.OgmiosAddress -> QueryM (Maybe Transaction.UtxoM)
    getUtxos address = convertUtxos <$> utxosAt' address

    convertUtxos :: Ogmios.UtxoQR -> Maybe Transaction.UtxoM
    convertUtxos (Ogmios.UtxoQR utxoQueryResult) =
      let
        out'
          :: Array
               ( Maybe Transaction.TransactionInput /\ Maybe
                   Transaction.TransactionOutput
               )
        out' = Map.toUnfoldable utxoQueryResult
          <#> bimap
            txOutRefToTransactionInput
            ogmiosTxOutToTransactionOutput

        out
          :: Maybe
               ( Array
                   ( Transaction.TransactionInput /\
                       Transaction.TransactionOutput
                   )
               )
        out = out' <#> bisequence # sequence
      in
        (wrap <<< Map.fromFoldable) <$> out

  -- Nami appear to remove collateral from the utxo set, so we shall do the same.
  -- This is crucial if we are submitting via Nami. If we decide to submit with
  -- Ogmios, we can remove this.
  -- More detail can be found here
  -- https://github.com/Berry-Pool/nami-wallet/blob/ecb32e39173b28d4a7a85b279a748184d4759f6f/src/api/extension/index.js
  -- by searching "// exclude collateral input from overall utxo set"
  -- or functions getUtxos and checkCollateral.
  namiUtxosAt :: Address -> QueryM (Maybe Transaction.UtxoM)
  namiUtxosAt address = do
    utxos' <- allUtxosAt address
    collateral' <- getWalletCollateral
    pure do
      utxos <- unwrap <$> utxos'
      collateral <- unwrap <$> collateral'
      pure $ wrap $ Map.delete collateral.input utxos

--------------------------------------------------------------------------------
-- Used Utxos helpers

filterUnusedUtxos :: UtxoM -> QueryM UtxoM
filterUnusedUtxos (UtxoM utxos) = withTxRefsCache $
  UtxoM <$> Helpers.filterMapWithKeyM (\k _ -> isTxOutRefUsed (unwrap k)) utxos

withTxRefsCache
  :: forall (m :: Type -> Type) (a :: Type)
   . ReaderT UsedTxOuts (LoggerT Aff) a
  -> QueryM a
withTxRefsCache f = withReaderT (_.usedTxOuts) f
