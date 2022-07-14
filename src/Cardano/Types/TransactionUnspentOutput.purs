module Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Types.Transaction (TransactionOutput)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Types.Transaction (TransactionInput)

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
