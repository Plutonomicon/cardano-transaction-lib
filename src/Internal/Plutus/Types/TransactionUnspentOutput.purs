module Ctl.Internal.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  , lookupTxHash
  , _input
  , _output
  , mkTxUnspentOut
  ) where

import Prelude

import Ctl.Internal.Plutus.Types.Transaction
  ( TransactionOutputWithRefScript
  , UtxoMap
  )
import Ctl.Internal.Types.Transaction (TransactionHash, TransactionInput)
import Data.Array (filter)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(Proxy))

newtype TransactionUnspentOutput = TransactionUnspentOutput
  { input :: TransactionInput
  , output :: TransactionOutputWithRefScript
  }

_input :: Lens' TransactionUnspentOutput TransactionInput
_input = _Newtype <<< prop (Proxy :: Proxy "input")

_output :: Lens' TransactionUnspentOutput TransactionOutputWithRefScript
_output = _Newtype <<< prop (Proxy :: Proxy "output")

derive instance Generic TransactionUnspentOutput _
derive instance Newtype TransactionUnspentOutput _
derive newtype instance Eq TransactionUnspentOutput

instance Show TransactionUnspentOutput where
  show = genericShow

lookupTxHash
  :: TransactionHash -> UtxoMap -> Array TransactionUnspentOutput
lookupTxHash txHash utxos =
  map (\(input /\ output) -> TransactionUnspentOutput { input, output })
    $ filter (fst >>> unwrap >>> _.transactionId >>> eq txHash)
    $
      Map.toUnfoldable utxos

mkTxUnspentOut
  :: TransactionInput
  -> TransactionOutputWithRefScript
  -> TransactionUnspentOutput
mkTxUnspentOut input output = TransactionUnspentOutput { input, output }
