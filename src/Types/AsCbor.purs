module Cardano.Types.AsCbor where

import Prelude

import Cardano.Serialization.Lib (class IsBytes, fromBytes, toBytes)
import Cardano.Serialization.Lib.Internal (class IsCsl)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)

-- AsCsl/AsCborBytes machinery

class
  IsCsl cslType <=
  AsCsl (cslType :: Type) (cardanoType :: Type)
  | cardanoType -> cslType
  , cslType -> cardanoType where
  fromCsl :: cslType -> cardanoType
  toCsl :: cardanoType -> cslType

-- this typeclass must be here for the code to pass orphan instance check
class AsCbor a where
  encodeCbor :: a -> CborBytes
  decodeCbor :: CborBytes -> Maybe a

encodeCborViaCsl
  :: forall csl cardano
   . IsBytes csl
  => IsCsl csl
  => AsCsl csl cardano
  => cardano
  -> CborBytes
encodeCborViaCsl = wrap <<< toBytes <<< toCsl

decodeCborViaCsl
  :: forall csl cardano
   . IsBytes csl
  => IsCsl csl
  => AsCsl csl cardano
  => CborBytes
  -> Maybe cardano
decodeCborViaCsl = map fromCsl <<< fromBytes <<< unwrap

encodeCbor'
  :: forall csl cardano
   . AsCsl csl cardano
  => IsBytes csl
  => cardano
  -> CborBytes
encodeCbor' = toCsl >>> toBytes >>> wrap

decodeCbor'
  :: forall csl cardano
   . AsCsl csl cardano
  => IsBytes csl
  => CborBytes
  -> Maybe cardano
decodeCbor' = unwrap >>> fromBytes >>> map fromCsl
