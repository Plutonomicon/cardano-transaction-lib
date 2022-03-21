module QueryM.Utxos
  ( utxosAt
  ) where

import Prelude
import Address (addressToOgmiosAddress)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (ask, asks)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bisequence)
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff (Canceler(Canceler), makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import QueryM
  ( QueryM
  , _stringify
  , _wsSend
  , allowError
  , getWalletCollateral
  , listeners
  , underlyingWebSocket
  )
import Serialization.Address (Address)
import Types.JsonWsp as JsonWsp
import Types.Transaction as Transaction
import TxOutput (ogmiosTxOutToTransactionOutput, txOutRefToTransactionInput)
import Wallet (Wallet(Nami))

-- the first query type in the QueryM/Aff interface
utxosAt' :: JsonWsp.OgmiosAddress -> QueryM JsonWsp.UtxoQR
utxosAt' addr = do
  body <- liftEffect $ JsonWsp.mkUtxosAtQuery { utxo: [ addr ] }
  let id = body.mirror.id
  sBody <- liftEffect $ _stringify body
  config <- ask
  -- not sure there's an easy way to factor this out unfortunately
  let
    affFunc :: (Either Error JsonWsp.UtxoQR -> Effect Unit) -> Effect Canceler
    affFunc cont = do
      let
        ls = listeners config.ogmiosWs
        ws = underlyingWebSocket config.ogmiosWs
      ls.utxo.addMessageListener id
        ( \result -> do
            ls.utxo.removeMessageListener id
            allowError cont $ result
        )
      _wsSend ws sBody
      pure $ Canceler $ \err -> do
        liftEffect $ ls.utxo.removeMessageListener id
        liftEffect $ throwError $ err
  liftAff $ makeAff $ affFunc

-- If required, we can change to Either with more granular error handling.
-- | Gets utxos at an (internal) `Address` in terms of (internal) `Transaction.Types`.
-- | Results may vary depending on `Wallet` type.
utxosAt :: Address -> QueryM (Maybe Transaction.UtxoM)
utxosAt addr = asks _.wallet >>= maybe (pure Nothing) (utxosAtByWallet addr)
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
    getUtxos :: JsonWsp.OgmiosAddress -> QueryM (Maybe Transaction.UtxoM)
    getUtxos address = convertUtxos <$> utxosAt' address

    convertUtxos :: JsonWsp.UtxoQR -> Maybe Transaction.UtxoM
    convertUtxos (JsonWsp.UtxoQR utxoQueryResult) =
      let
        out' :: Array (Maybe Transaction.TransactionInput /\ Maybe Transaction.TransactionOutput)
        out' = Map.toUnfoldable utxoQueryResult
          <#> bimap
            txOutRefToTransactionInput
            ogmiosTxOutToTransactionOutput

        out :: Maybe (Array (Transaction.TransactionInput /\ Transaction.TransactionOutput))
        out = out' <#> bisequence # sequence
      in
        (wrap <<< Map.fromFoldable) <$> out

  -- Nami appear to remove collateral from the utxo set, so we shall do the same.
  -- This is crucial if we are submitting via Nami. If we decide to submit with
  -- Ogmios, we can remove this.
  -- More detail can be found here https://github.com/Berry-Pool/nami-wallet/blob/ecb32e39173b28d4a7a85b279a748184d4759f6f/src/api/extension/index.js
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
