module Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  , transactionUnspentOutputsToUtxoMap
  , fromCsl
  ) where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Serialization.Lib
  ( transactionUnspentOutput_input
  , transactionUnspentOutput_new
  , transactionUnspentOutput_output
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionInput as TransactionInput
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.TransactionOutput as TransactionOutput
import Cardano.Types.UtxoMap (UtxoMap)
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
  TransactionUnspentOutput { input, output }

toCsl :: TransactionUnspentOutput -> Csl.TransactionUnspentOutput
toCsl (TransactionUnspentOutput { input, output }) =
  transactionUnspentOutput_new (TransactionInput.toCsl input)
    (TransactionOutput.toCsl output)
