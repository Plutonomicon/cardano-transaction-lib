module Internal.CardanoCli.QueryHandle
  ( withCardanoCliCompletion
  ) where

import Prelude

import Cardano.Types.Address (Address)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.UtxoMap (UtxoMap)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader (local)
import Ctl.Internal.CardanoCli
  ( CardanoCliTxOutInfo
  , CardanoNodeInstance
  , cardanoCliTxOutInfoToUtxo
  , queryUtxosViaCardanoCli
  ) as CardanoCli
import Ctl.Internal.Contract.Monad (Contract, ContractEnv)
import Ctl.Internal.Service.Error (ClientError(ClientOtherError))
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Lens (Lens', (%~))
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Effect.Exception (message) as Error
import Type.Proxy (Proxy(Proxy))

type UtxosAtQuery = Address -> Aff (Either ClientError UtxoMap)

type GetUtxoByOrefQuery =
  TransactionInput -> Aff (Either ClientError (Maybe TransactionOutput))

utxosAtLens :: Lens' ContractEnv UtxosAtQuery
utxosAtLens =
  prop (Proxy :: _ "handle")
    <<< prop (Proxy :: _ "utxosAt")

getUtxoByOrefLens :: Lens' ContractEnv GetUtxoByOrefQuery
getUtxoByOrefLens =
  prop (Proxy :: _ "handle")
    <<< prop (Proxy :: _ "getUtxoByOref")

withCardanoCliCompletion
  :: forall a
   . CardanoCli.CardanoNodeInstance
  -> Address
  -> Contract a
  -> Contract a
withCardanoCliCompletion node genesisAddr =
  local $ (utxosAtLens %~ completeUtxosAt node) >>>
    (getUtxoByOrefLens %~ completeGetUtxoByOref node genesisAddr)

-- | Complements the `utxosAt` result with utxos found via cardano-cli.
-- | In case of overlapping results, the utxos from the query layer are given
-- | preference.
-- |
-- | NOTE: It is assumed that utxos retrieved via cardano-cli do not include
-- | datum or reference scripts.
completeUtxosAt
  :: CardanoCli.CardanoNodeInstance -> UtxosAtQuery -> UtxosAtQuery
completeUtxosAt node utxosAt address =
  runExceptT do
    queryLayerUtxos <- ExceptT $ utxosAt address
    cardanoCliUtxos <- ExceptT $ queryUtxosViaCardanoCli node address
    pure $ Map.union queryLayerUtxos cardanoCliUtxos

-- | Complements the `getUtxoByOref` search space with utxos found via
-- | cardano-cli. If no utxo is found in the initial search, the lookup will be
-- | performed using utxos from cardano-cli.
-- |
-- | NOTE: It is assumed that utxos retrieved via cardano-cli do not include
-- | datum or reference scripts.
completeGetUtxoByOref
  :: CardanoCli.CardanoNodeInstance
  -> Address
  -> GetUtxoByOrefQuery
  -> GetUtxoByOrefQuery
completeGetUtxoByOref node address getUtxoByOref oref =
  runExceptT do
    mbUtxo <- ExceptT $ getUtxoByOref oref
    cardanoCliUtxos <- ExceptT $ queryUtxosViaCardanoCli node address
    pure $ mbUtxo <|> Map.lookup oref cardanoCliUtxos

queryUtxosViaCardanoCli
  :: CardanoCli.CardanoNodeInstance
  -> Address
  -> Aff (Either ClientError UtxoMap)
queryUtxosViaCardanoCli node address =
  bimap toClientError toUtxoMap <$>
    try (CardanoCli.queryUtxosViaCardanoCli node address)
  where
  toClientError :: Error -> ClientError
  toClientError = ClientOtherError <<< Error.message

  toUtxoMap :: Array CardanoCli.CardanoCliTxOutInfo -> UtxoMap
  toUtxoMap =
    Map.fromFoldable
      <<< map (CardanoCli.cardanoCliTxOutInfoToUtxo address)
