module Contract.Crypto
  ( plutusHash
  , HashMethod(..)
  ) where

import Prelude

import Contract.Monad (Contract, wrapContract)
import Data.Either (Either)
import QueryM.Crypto as Crypto
import Types.ByteArray (ByteArray)

type HashMethod = Crypto.HashMethod

plutusHash
  :: forall (r :: Row Type)
   . HashMethod
  -> ByteArray
  -> Contract r (Either String ByteArray)
plutusHash meth = wrapContract <<< Crypto.plutusHash meth
