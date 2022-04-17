module Plutus.Types.AddressHeaderType
  ( AddressHeaderType(..)
  , addrHeaderTypeUInt
  , addrHeaderType
  ) where

import Prelude (class Eq, class Ord, (<<<))
import Data.Maybe (Maybe)
import Data.UInt (UInt, fromInt, toInt)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Enum (class Enum, class BoundedEnum, toEnum, fromEnum)
import Data.Bounded (class Bounded)
import Data.Enum.Generic
  ( genericPred
  , genericSucc
  , genericCardinality
  , genericToEnum
  , genericFromEnum
  )

-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0019#shelley-addresses
-- | The various types of Shelley address headers described in CIP-0019.
data AddressHeaderType
  = PaymentKeyHash_StakeKeyHash
  | ScriptHash_StakeKeyHash
  | PaymentKeyHash_ScriptHash
  | ScriptHash_ScriptHash
  | PaymentKeyHash_Pointer
  | ScriptHash_Pointer
  | PaymentKeyHash
  | ScriptHash

derive instance Eq AddressHeaderType
derive instance Ord AddressHeaderType
derive instance Generic AddressHeaderType _

instance Show AddressHeaderType where
  show = genericShow

instance Enum AddressHeaderType where
  pred = genericPred
  succ = genericSucc

instance Bounded AddressHeaderType where
  top = ScriptHash
  bottom = PaymentKeyHash_StakeKeyHash

instance BoundedEnum AddressHeaderType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

addrHeaderTypeUInt :: AddressHeaderType -> UInt
addrHeaderTypeUInt = fromInt <<< fromEnum

addrHeaderType :: UInt -> Maybe AddressHeaderType
addrHeaderType = toEnum <<< toInt
