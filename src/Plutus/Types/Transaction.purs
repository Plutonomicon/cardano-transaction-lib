module Plutus.Types.Transaction
  ( TransactionOutput(..)
  , Utxo
  , UtxoM(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import FromData (class FromData, fromData)
import Plutus.Types.Address (Address)
import Plutus.Types.Value (Value)
import ToData (class ToData, toData)
import Types.PlutusData (PlutusData(Constr))
import Types.Transaction (TransactionInput, TxOutput(TxOutput))

newtype TransactionOutput = TransactionOutput (TxOutput Address Value)

derive instance Generic TransactionOutput _
derive instance Newtype TransactionOutput _
derive newtype instance Eq TransactionOutput

instance Show TransactionOutput where
  show = genericShow

instance FromData TransactionOutput where
  fromData (Constr n [ addr, amt, dh ]) | n == zero =
    TransactionOutput <<< TxOutput <$>
      ( { address: _, amount: _, dataHash: _ }
          <$> fromData addr
          <*> fromData amt
          <*> fromData dh
      )
  fromData _ = Nothing

instance ToData TransactionOutput where
  toData (TransactionOutput (TxOutput { address, amount, dataHash })) =
    Constr zero [ toData address, toData amount, toData dataHash ]

newtype UtxoM = UtxoM Utxo

derive instance Generic UtxoM _
derive instance Newtype UtxoM _
derive newtype instance Eq UtxoM

instance Show UtxoM where
  show = genericShow

type Utxo = Map TransactionInput TransactionOutput
