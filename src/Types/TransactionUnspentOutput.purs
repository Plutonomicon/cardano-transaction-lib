module Types.TransactionUnspentOutput where

import Types.Transaction (TransactionInput, TransactionOutput)
import Data.Show.Generic (genericShow)
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype TransactionUnspentOutput = TransactionUnspentOutput
  { input :: TransactionInput
  , output :: TransactionOutput
  }

derive instance Generic TransactionUnspentOutput _
derive instance Newtype TransactionUnspentOutput _
derive newtype instance Eq TransactionUnspentOutput

instance Show TransactionUnspentOutput where
  show = genericShow
