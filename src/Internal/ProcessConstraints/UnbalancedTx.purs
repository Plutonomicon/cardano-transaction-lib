module Ctl.Internal.ProcessConstraints.UnbalancedTx
  ( UnbalancedTx(UnbalancedTx)
  ) where

import Prelude hiding (join)

import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.Transaction (Transaction)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Ctl.Internal.BalanceTx.RedeemerIndex (UnindexedRedeemer)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- | A newtype for the unbalanced transaction after creating one with datums
-- | and redeemers not attached.
newtype UnbalancedTx = UnbalancedTx
  { transaction :: Transaction -- the unbalanced tx created
  , usedUtxos :: Map TransactionInput TransactionOutput
  , datums ::
      Array PlutusData -- the array of ordered datums that require attaching
  , redeemers :: Array UnindexedRedeemer
  }

derive instance Generic UnbalancedTx _
derive instance Newtype UnbalancedTx _
derive newtype instance Eq UnbalancedTx
-- derive newtype instance EncodeAeson UnbalancedTx

instance Show UnbalancedTx where
  show = genericShow
