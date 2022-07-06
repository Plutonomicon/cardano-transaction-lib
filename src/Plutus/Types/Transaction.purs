module Plutus.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  , Utxo
  , UtxoM(UtxoM)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import FromData (class FromData, fromData, genericFromData)
import Plutus.Types.Address (Address)
import Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , PNil
  )
import Plutus.Types.Value (Value)
import Serialization.Hash (ScriptHash)
import ToData (class ToData, genericToData, toData)
import TypeLevel.Nat (S, Z)
import Types.Datum (Datum)
import Types.PlutusData (PlutusData(Constr))
import Types.Transaction (DataHash, TransactionInput)

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

data OutputDatum = NoOutputDatum | OutputDatumHash DataHash | OutputDatum Datum

derive instance Generic OutputDatum _
derive instance Eq OutputDatum

instance Show OutputDatum where
  show = genericShow

instance
  HasPlutusSchema OutputDatum
    ( "NoOutputDatum" := PNil @@ Z
        :+ "OutputDatumHash"
        := PNil
        @@ (S Z)
        :+ "OutputDatum"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData OutputDatum where
  toData = genericToData

instance FromData OutputDatum where
  fromData = genericFromData

newtype UtxoM = UtxoM Utxo

derive instance Generic UtxoM _
derive instance Newtype UtxoM _
derive newtype instance Eq UtxoM

instance Show UtxoM where
  show = genericShow

type Utxo = Map TransactionInput TransactionOutput
