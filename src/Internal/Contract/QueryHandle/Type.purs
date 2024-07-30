module Ctl.Internal.Contract.QueryHandle.Type
  ( QueryHandle
  , AffE
  ) where

import Cardano.Types
  ( Address
  , DataHash
  , NetworkId
  , PlutusData
  , PoolPubKeyHash
  , ScriptHash
  , ScriptRef
  , StakePubKeyHash
  , Transaction
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  , UtxoMap
  )
import Cardano.Types.AuxiliaryData (AuxiliaryData)
import Ctl.Internal.Contract.QueryHandle.Error (GetTxMetadataError)
import Ctl.Internal.QueryM.Ogmios
  ( AdditionalUtxoSet
  , CurrentEpoch
  , TxEvaluationR
  )
import Ctl.Internal.Service.Error (ClientError)
import Ctl.Internal.Types.Chain (Tip) as Chain
import Ctl.Internal.Types.DelegationsAndRewards (DelegationsAndRewards)
import Ctl.Internal.Types.EraSummaries (EraSummaries)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

type AffE (a :: Type) = Aff (Either ClientError a)

type QueryHandle =
  { getDatumByHash :: DataHash -> AffE (Maybe PlutusData)
  , getScriptByHash :: ScriptHash -> AffE (Maybe ScriptRef)
  , getTxAuxiliaryData ::
      TransactionHash
      -> Aff (Either GetTxMetadataError AuxiliaryData)
  , getUtxoByOref :: TransactionInput -> AffE (Maybe TransactionOutput)
  , getOutputAddressesByTxHash :: TransactionHash -> AffE (Array Address)
  , doesTxExist :: TransactionHash -> AffE Boolean
  , utxosAt :: Address -> AffE UtxoMap
  , getChainTip :: AffE Chain.Tip
  , getCurrentEpoch :: Aff CurrentEpoch
  -- TODO Capture errors from all backends
  , submitTx :: Transaction -> AffE TransactionHash
  , evaluateTx :: Transaction -> AdditionalUtxoSet -> Aff TxEvaluationR
  , getEraSummaries :: AffE EraSummaries
  , getPoolIds :: AffE (Array PoolPubKeyHash)
  , getPubKeyHashDelegationsAndRewards ::
      NetworkId -> StakePubKeyHash -> AffE (Maybe DelegationsAndRewards)
  , getValidatorHashDelegationsAndRewards ::
      NetworkId -> ScriptHash -> AffE (Maybe DelegationsAndRewards)
  }
