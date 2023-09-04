module Ctl.Internal.Contract.QueryHandle.Type
  ( QueryHandle
  , AffE
  ) where

import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef)
import Ctl.Internal.Cardano.Types.Transaction
  ( PoolPubKeyHash
  , Transaction
  , TransactionOutput
  , UtxoMap
  )
import Ctl.Internal.Contract.QueryHandle.Error (GetTxMetadataError)
import Ctl.Internal.QueryM.Ogmios
  ( AdditionalUtxoSet
  , CurrentEpoch
  , TxEvaluationR
  )
import Ctl.Internal.QueryM.Pools (DelegationsAndRewards)
import Ctl.Internal.Serialization.Address (Address, NetworkId)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Service.Error (ClientError)
import Ctl.Internal.Types.Chain as Chain
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Ctl.Internal.Types.EraSummaries (EraSummaries)
import Ctl.Internal.Types.PubKeyHash (StakePubKeyHash)
import Ctl.Internal.Types.Scripts (StakeValidatorHash)
import Ctl.Internal.Types.Transaction (TransactionHash, TransactionInput)
import Ctl.Internal.Types.TransactionMetadata (GeneralTransactionMetadata)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

type AffE (a :: Type) = Aff (Either ClientError a)

type QueryHandle =
  { getDatumByHash :: DataHash -> AffE (Maybe Datum)
  , getScriptByHash :: ScriptHash -> AffE (Maybe ScriptRef)
  , getTxMetadata ::
      TransactionHash
      -> Aff (Either GetTxMetadataError GeneralTransactionMetadata)
  , getUtxoByOref :: TransactionInput -> AffE (Maybe TransactionOutput)
  , getOutputAddressesByTxHash :: TransactionHash -> AffE (Array Address)
  , doesTxExist :: TransactionHash -> AffE Boolean
  , utxosAt :: Address -> AffE UtxoMap
  , utxosAtScriptHash :: ScriptHash -> AffE UtxoMap
  , getChainTip :: AffE Chain.Tip
  , getCurrentEpoch :: Aff CurrentEpoch
  -- TODO Capture errors from all backends
  , submitTx :: Transaction -> Aff (Either ClientError TransactionHash)
  , evaluateTx :: Transaction -> AdditionalUtxoSet -> Aff TxEvaluationR
  , getEraSummaries :: AffE EraSummaries
  , getPoolIds :: AffE (Array PoolPubKeyHash)
  , getPubKeyHashDelegationsAndRewards ::
      NetworkId -> StakePubKeyHash -> AffE (Maybe DelegationsAndRewards)
  , getValidatorHashDelegationsAndRewards ::
      NetworkId -> StakeValidatorHash -> AffE (Maybe DelegationsAndRewards)
  }
