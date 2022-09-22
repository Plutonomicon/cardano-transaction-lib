module Ctl.Plutus.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoMap
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef)
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.OutputDatum (OutputDatum)
import Ctl.Internal.Types.PlutusData (PlutusData(Constr))
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Plutus.Types.Address (Address)
import Ctl.Plutus.Types.Value (Value)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

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

type UtxoMap = Map TransactionInput TransactionOutputWithRefScript
