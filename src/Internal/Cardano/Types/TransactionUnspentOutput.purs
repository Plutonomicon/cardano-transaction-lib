module Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  , transactionUnspentOutputsToUtxoMap
  ) where

import Prelude

import Aeson (class EncodeAeson)
import Ctl.Internal.Cardano.Types.Transaction (TransactionOutput, UtxoMap)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))

newtype TransactionUnspentOutput = TransactionUnspentOutput
  { input :: TransactionInput
  , output :: TransactionOutput
  }

derive instance Generic TransactionUnspentOutput _
derive instance Newtype TransactionUnspentOutput _
derive newtype instance Eq TransactionUnspentOutput
derive newtype instance EncodeAeson TransactionUnspentOutput

instance Show TransactionUnspentOutput where
  show = genericShow

transactionUnspentOutputsToUtxoMap :: Array TransactionUnspentOutput -> UtxoMap
transactionUnspentOutputsToUtxoMap = Map.fromFoldable <<< map
  \(TransactionUnspentOutput { input, output }) -> Tuple input output
