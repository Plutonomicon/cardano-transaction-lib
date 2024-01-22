module Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  , transactionUnspentOutputsToUtxoMap
  ) where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Serialization.Lib (transactionUnspentOutput_input)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionInput as TransactionInput
import Ctl.Internal.Cardano.Types.Transaction (TransactionOutput, UtxoMap)
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

fromCsl
  :: Csl.TransactionUnspentOutput -> TransactionUnspentOutput
fromCsl tuo = do
  let
    input = TransactionInput.fromCsl $ transactionUnspentOutput_input tuo
    output = TransactionOutput.fromCsl $ transactionUnspentOutput_output tuo
  T.TransactionUnspentOutput { input, output }
