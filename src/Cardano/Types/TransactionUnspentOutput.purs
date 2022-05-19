module Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(..)
  ) where

import Prelude

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

instance Show TransactionUnspentOutput where
  show = genericShow
