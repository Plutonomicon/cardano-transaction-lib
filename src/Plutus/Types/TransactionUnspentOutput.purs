module Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  , mkTxUnspentOut
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Plutus.Types.Transaction (TransactionOutputWithRefScript)
import Types.Transaction (TransactionInput)

newtype TransactionUnspentOutput = TransactionUnspentOutput
  { input :: TransactionInput
  , output :: TransactionOutputWithRefScript
  }

derive instance Generic TransactionUnspentOutput _
derive instance Newtype TransactionUnspentOutput _
derive newtype instance Eq TransactionUnspentOutput

instance Show TransactionUnspentOutput where
  show = genericShow

mkTxUnspentOut
  :: TransactionInput
  -> TransactionOutputWithRefScript
  -> TransactionUnspentOutput
mkTxUnspentOut input output = TransactionUnspentOutput { input, output }
