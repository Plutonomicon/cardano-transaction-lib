module CTL.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) where

import Prelude

import Aeson (class EncodeAeson)
import CTL.Internal.Cardano.Types.Transaction (TransactionOutput)
import CTL.Internal.Types.Transaction (TransactionInput)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

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
