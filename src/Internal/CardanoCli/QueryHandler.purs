module Internal.CardanoCli.QueryHandler
  ( withCardanoCliCompletion
  ) where

import Contract.Prelude

import Cardano.Types as Cardano.Types
import Cardano.Types.Address (Address)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Contract.ClientError as Contract.ClientError
import Contract.Monad (Contract, ContractEnv)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader (local)
import Ctl.Internal.CardanoCli as CardanoCli
import Data.Bifunctor (bimap)
import Data.Lens (Lens', (%~))
import Data.Lens.Record (prop)
import Data.Map as Map
import Effect.Exception (Error, message)
import Type.Proxy (Proxy(Proxy))

type UtxosAtQuery =
  Cardano.Types.Address
  -> Aff (Either Contract.ClientError.ClientError Cardano.Types.UtxoMap)

type GetUtxoByOrefQuery =
  TransactionInput
  -> Aff (Either Contract.ClientError.ClientError (Maybe TransactionOutput))

-- | Adds to the utxosAt results UTxOs found by cardano-cli but not found by the current 'utxosAt' query.
-- UTxOs found by cardano-cli assumed to have no datum or script ref.
withCardanoCliCompletion
  :: forall a
   . CardanoCli.CardanoNodeInstance
  -> Address
  -> Contract a
  -> Contract a
withCardanoCliCompletion node genesisAddr =
  local $ (utxosAtL %~ completeUtxosAt node) >>>
    (getUtxoByOrefL %~ completeGetUtxoByOref node genesisAddr)

utxosAtL :: Lens' ContractEnv UtxosAtQuery
utxosAtL = prop (Proxy :: _ "handle") <<< prop (Proxy :: _ "utxosAt")

getUtxoByOrefL :: Lens' ContractEnv GetUtxoByOrefQuery
getUtxoByOrefL = prop (Proxy :: _ "handle") <<< prop
  (Proxy :: _ "getUtxoByOref")

-- | Adds to the results UTxOs found by cardano-cli but not found by the given 'utxosAt' query.
-- UTxOs found by cardano-cli assumed to have no datum or script ref.
completeUtxosAt
  :: CardanoCli.CardanoNodeInstance
  -> UtxosAtQuery
  -> UtxosAtQuery
completeUtxosAt node utxosAt address = runExceptT do
  let
    toCliError :: Error -> Contract.ClientError.ClientError
    toCliError = Contract.ClientError.ClientOtherError <<< message

    toUtxoMap :: Array CardanoCli.CardanoCliTxOutInfo -> Cardano.Types.UtxoMap
    toUtxoMap = Map.fromFoldable
      <<< map (CardanoCli.cardanoCliTxOutInfoToUtxo address)
  cardanoCliUtxos <- ExceptT
    $ map (bimap toCliError toUtxoMap)
    $ try
    $ CardanoCli.queryUtxosViaCardanoCli node address
  kupoUtxos <- ExceptT $ utxosAt address
  pure $ Map.union kupoUtxos cardanoCliUtxos

-- | Adds to the results UTxOs found by cardano-cli but not found by the given 'getUtxoByOref' query.
-- UTxOs found by cardano-cli assumed to have no datum or script ref.
completeGetUtxoByOref -- FIXME
  :: CardanoCli.CardanoNodeInstance
  -> Address
  -> GetUtxoByOrefQuery
  -> GetUtxoByOrefQuery
completeGetUtxoByOref node address getUtxoByOref oref = runExceptT do
  let
    toCliError :: Error -> Contract.ClientError.ClientError
    toCliError = Contract.ClientError.ClientOtherError <<< message

    toUtxoMap :: Array CardanoCli.CardanoCliTxOutInfo -> Cardano.Types.UtxoMap
    toUtxoMap = Map.fromFoldable
      <<< map (CardanoCli.cardanoCliTxOutInfoToUtxo address)
  cardanoCliUtxos <- ExceptT
    $ map (bimap toCliError toUtxoMap)
    $ try
    $ CardanoCli.queryUtxosViaCardanoCli node address
  mUtxo <- ExceptT $ getUtxoByOref oref
  pure $ mUtxo <|> Map.lookup oref cardanoCliUtxos
