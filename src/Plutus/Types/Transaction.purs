module Plutus.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , Utxo
  , UtxoM(UtxoM)
  ) where

import Prelude

import Cardano.Types.ScriptRef (ScriptRef)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import FromData (class FromData, fromData)
import Plutus.Types.Address (Address)
import Plutus.Types.Value (Value)
import Serialization.Hash (ScriptHash)
import ToData (class ToData, toData)
import Types.PlutusData (PlutusData(Constr))
import Types.Transaction (TransactionInput)
import Types.OutputDatum (OutputDatum)

-- https://github.com/input-output-hk/plutus/blob/c8d4364d0e639fef4d5b93f7d6c0912d992b54f9/plutus-ledger-api/src/PlutusLedgerApi/V2/Tx.hs#L80
newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , datum :: OutputDatum
  , referenceScript :: Maybe ScriptHash
  }

derive instance Generic TransactionOutput _
derive instance Newtype TransactionOutput _
derive newtype instance Eq TransactionOutput

instance Show TransactionOutput where
  show = genericShow

instance FromData TransactionOutput where
  fromData (Constr n [ addr, amt, datum, referenceScript ]) | n == zero =
    TransactionOutput <$>
      ( { address: _, amount: _, datum: _, referenceScript: _ }
          <$> fromData addr
          <*> fromData amt
          <*> fromData datum
          <*> fromData referenceScript
      )
  fromData _ = Nothing

instance ToData TransactionOutput where
  toData (TransactionOutput { address, amount, datum, referenceScript }) =
    Constr zero
      [ toData address, toData amount, toData datum, toData referenceScript ]

newtype TransactionOutputWithRefScript = TransactionOutputWithRefScript
  { output :: TransactionOutput
  , scriptRef :: Maybe ScriptRef
  }

derive instance Generic TransactionOutputWithRefScript _
derive instance Newtype TransactionOutputWithRefScript _
derive newtype instance Eq TransactionOutputWithRefScript

instance Show TransactionOutputWithRefScript where
  show = genericShow

instance FromData TransactionOutputWithRefScript where
  fromData = map (wrap <<< { output: _, scriptRef: Nothing }) <<< fromData

instance ToData TransactionOutputWithRefScript where
  toData = toData <<< _.output <<< unwrap

newtype UtxoM = UtxoM Utxo

derive instance Generic UtxoM _
derive instance Newtype UtxoM _
derive newtype instance Eq UtxoM

instance Show UtxoM where
  show = genericShow

type Utxo = Map TransactionInput TransactionOutputWithRefScript
