module Contract.Crypto
  ( hashData
  , hashScript
  , datumHash
  , plutusHash
  , HashedData(..)
  , HashMethod(..)
  ) where

import Contract.Monad (Contract, wrapContract)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Prelude ((<<<))
import QueryM (ClientError)
import QueryM.Crypto as Crypt
import Serialization.Hash (ScriptHash)
import Types.ByteArray (ByteArray)
import Types.Datum (Datum, DatumHash)
import Types.Scripts (PlutusScript)
import Data.Newtype (class Newtype)

type HashMethod = Crypt.HashMethod
type HashedData = Crypt.HashedData

plutusHash
  :: forall (r :: Row Type)
   . HashMethod
  -> ByteArray
  -> Contract r (Maybe ByteArray)
plutusHash meth = wrapContract <<< Crypt.plutusHash meth

hashData :: forall (r :: Row Type). Datum -> Contract r (Maybe HashedData)
hashData = wrapContract <<< Crypt.hashData

hashScript
  :: forall (a :: Type) (b :: Type) (r :: Row Type)
   . Newtype a PlutusScript
  => Newtype b ScriptHash
  => a
  -> Contract r (Either ClientError b)
hashScript = wrapContract <<< Crypt.hashScript

datumHash :: forall (r :: Row Type). Datum -> Contract r (Maybe DatumHash)
datumHash = wrapContract <<< Crypt.datumHash
