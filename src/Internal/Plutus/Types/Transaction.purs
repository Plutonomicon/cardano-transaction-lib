module Ctl.Internal.Plutus.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoMap
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , _amount
  , _datum
  , _output
  , _scriptRef
  , pprintTransactionOutput
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef)
import Ctl.Internal.Cardano.Types.Value (pprintValue)
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.Plutus.Conversion.Value (fromPlutusValue)
import Ctl.Internal.Plutus.Types.Address (Address)
import Ctl.Internal.Plutus.Types.Value (Value)
import Ctl.Internal.Serialization.Hash (ScriptHash, scriptHashToBytes)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.OutputDatum (OutputDatum, pprintOutputDatum)
import Ctl.Internal.Types.PlutusData (PlutusData(Constr))
import Ctl.Internal.Types.RawBytes (rawBytesToHex)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(Proxy))

-- https://github.com/input-output-hk/plutus/blob/c8d4364d0e639fef4d5b93f7d6c0912d992b54f9/plutus-ledger-api/src/PlutusLedgerApi/V2/Tx.hs#L80
newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , datum :: OutputDatum
  , referenceScript :: Maybe ScriptHash
  }

_amount :: Lens' TransactionOutput Value
_amount = _Newtype <<< prop (Proxy :: Proxy "amount")

_datum :: Lens' TransactionOutput OutputDatum
_datum = _Newtype <<< prop (Proxy :: Proxy "datum")

derive instance Generic TransactionOutput _
derive instance Newtype TransactionOutput _
derive newtype instance Eq TransactionOutput

instance Show TransactionOutput where
  show = genericShow

derive newtype instance DecodeAeson TransactionOutput
derive newtype instance EncodeAeson TransactionOutput

instance FromData TransactionOutput where
  fromData (Constr n [ addr, amt, datum, referenceScript ])
    | n == BigNum.zero =
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
    Constr BigNum.zero
      [ toData address, toData amount, toData datum, toData referenceScript ]

pprintTransactionOutput :: TransactionOutput -> TagSet
pprintTransactionOutput
  (TransactionOutput { address, amount, datum, referenceScript }) =
  TagSet.fromArray $
    [ "address" `tag` show address
    , "amount" `tagSetTag` pprintValue (fromPlutusValue amount)
    , pprintOutputDatum datum
    ] <> referenceScriptTagSet
  where
  referenceScriptTagSet = maybe []
    (pure <<< tag "referenceScript" <<< rawBytesToHex <<< scriptHashToBytes)
    referenceScript

newtype TransactionOutputWithRefScript = TransactionOutputWithRefScript
  { output :: TransactionOutput
  , scriptRef :: Maybe ScriptRef
  }

_output :: Lens' TransactionOutputWithRefScript TransactionOutput
_output = _Newtype <<< prop (Proxy :: Proxy "output")

_scriptRef :: Lens' TransactionOutputWithRefScript (Maybe ScriptRef)
_scriptRef = _Newtype <<< prop (Proxy :: Proxy "scriptRef")

derive instance Generic TransactionOutputWithRefScript _
derive instance Newtype TransactionOutputWithRefScript _
derive newtype instance Eq TransactionOutputWithRefScript

instance Show TransactionOutputWithRefScript where
  show = genericShow

derive newtype instance DecodeAeson TransactionOutputWithRefScript
derive newtype instance EncodeAeson TransactionOutputWithRefScript

instance FromData TransactionOutputWithRefScript where
  fromData = map (wrap <<< { output: _, scriptRef: Nothing }) <<< fromData

instance ToData TransactionOutputWithRefScript where
  toData = toData <<< _.output <<< unwrap

type UtxoMap = Map TransactionInput TransactionOutputWithRefScript
