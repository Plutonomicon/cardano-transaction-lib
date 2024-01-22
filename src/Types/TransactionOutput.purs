module Cardano.Types.TransactionOutput where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Types.Address (Address)
import Cardano.Types.OutputDatum (OutputDatum(..))
import Cardano.Types.ScriptRef (ScriptRef)
import Cardano.Types.Value (Value(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)


newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , datum :: OutputDatum
  , scriptRef :: Maybe ScriptRef
  }

derive instance Generic TransactionOutput _
derive instance Newtype TransactionOutput _
derive newtype instance Eq TransactionOutput
derive newtype instance EncodeAeson TransactionOutput

instance Show TransactionOutput where
  show = genericShow
