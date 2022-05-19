module Contract.Crypto
  ( hashData
  , hashScript
  , datumHash
  , plutusHash
  , HashedData(..)
  , HashMethod(..)
  ) where

import Prelude

import Contract.Monad (Contract, wrapContract)
import Data.Either (Either)
import Data.Maybe (Maybe)
import QueryM (ClientError)
import QueryM.Crypto as Crypto
import Serialization.Hash (ScriptHash)
import Types.ByteArray (ByteArray)
import Types.Datum (Datum, DatumHash)
import Types.Scripts (PlutusScript)
import Data.Newtype (class Newtype)

type HashMethod = Crypto.HashMethod
type HashedData = Crypto.HashedData

plutusHash
  :: forall (r :: Row Type)
   . HashMethod
  -> ByteArray
  -> Contract r (Maybe ByteArray)
plutusHash meth = wrapContract <<< Crypto.plutusHash meth

hashData :: forall (r :: Row Type). Datum -> Contract r (Maybe HashedData)
hashData = wrapContract <<< Crypto.hashData

hashScript
  :: forall (a :: Type) (b :: Type) (r :: Row Type)
   . Newtype a PlutusScript
  => Newtype b ScriptHash
  => a
  -> Contract r (Either ClientError b)
hashScript = wrapContract <<< Crypto.hashScript

datumHash :: forall (r :: Row Type). Datum -> Contract r (Maybe DatumHash)
datumHash = wrapContract <<< Crypto.datumHash
