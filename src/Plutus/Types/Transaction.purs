module Plutus.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoMap
  , lookupTxHash
  ) where

import Prelude

import Data.Array (filter)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\))
import FromData (class FromData, fromData)
import Plutus.Types.Address (Address)
import Plutus.Types.Value (Value)
import ToData (class ToData, toData)
import Types.PlutusData (PlutusData(Constr))
import Types.Transaction (DataHash, TransactionHash, TransactionInput)

newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , dataHash :: Maybe DataHash
  }

derive instance Generic TransactionOutput _
derive instance Newtype TransactionOutput _
derive newtype instance Eq TransactionOutput

instance Show TransactionOutput where
  show = genericShow

instance FromData TransactionOutput where
  fromData (Constr n [ addr, amt, dh ]) | n == zero =
    TransactionOutput <$>
      ( { address: _, amount: _, dataHash: _ }
          <$> fromData addr
          <*> fromData amt
          <*> fromData dh
      )
  fromData _ = Nothing

instance ToData TransactionOutput where
  toData (TransactionOutput { address, amount, dataHash }) =
    Constr zero [ toData address, toData amount, toData dataHash ]

type UtxoMap = Map TransactionInput TransactionOutput

lookupTxHash
  :: TransactionHash -> UtxoMap -> Array (TransactionInput /\ TransactionOutput)
lookupTxHash txHash utxos =
  filter (fst >>> unwrap >>> _.transactionId >>> eq txHash) $
    Map.toUnfoldable utxos
