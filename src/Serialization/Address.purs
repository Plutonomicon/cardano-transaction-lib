module Serialization.Address (
  BaseAddress,
  Bech32String(..),
  PubKeyHash,
  StakeKeyHash,
  NetworkId,
  addressBech32,
  addressNetworkId,
  addressPubKeyHash,
  addressStakeKeyHash,
  bech32String,
  fromBech32,
  newBaseAddress
  ) where

import Data.Maybe (Maybe(Nothing, Just))
import Prelude


foreign import data BaseAddress :: Type

instance showBaseAddr :: Show BaseAddress where
  show addr = "(BaseAddress " <> show (addressBech32 addr) <> ")"

instance eqBaseAddr :: Eq BaseAddress where
  eq a1 a2 = addressBech32 a1 == addressBech32 a2


foreign import addressBech32 :: BaseAddress -> Bech32String

foreign import addressNetworkId :: BaseAddress -> NetworkId

foreign import addressPubKeyHashImpl ::
  (forall x. x -> Maybe x) ->
  (forall x. Maybe x) ->
  BaseAddress ->
  Maybe PubKeyHash

foreign import addressStakeKeyHashImpl ::
  (forall x. x -> Maybe x) ->
  (forall x. Maybe x) ->
  BaseAddress ->
  Maybe StakeKeyHash

foreign import fromBech32Impl :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Bech32String -> Maybe BaseAddress

foreign import newBaseAddress :: NetworkId -> PubKeyHash -> StakeKeyHash -> BaseAddress


newtype Bech32String = Bech32String String

derive newtype instance showBech32String :: Show Bech32String
derive instance eqBech32String :: Eq Bech32String

newtype PubKeyHash = PubKeyHash Bech32String

derive newtype instance showPubKeyHash :: Show PubKeyHash

newtype StakeKeyHash = StakeKeyHash Bech32String

derive newtype instance showStakeKeyHash :: Show StakeKeyHash

newtype NetworkId = NetworkId Number

derive newtype instance showNetworkId :: Show NetworkId


bech32String :: Bech32String -> String
bech32String (Bech32String str) = str

fromBech32 :: Bech32String -> Maybe BaseAddress
fromBech32 = fromBech32Impl Just Nothing

addressPubKeyHash :: BaseAddress -> Maybe PubKeyHash
addressPubKeyHash = addressPubKeyHashImpl Just Nothing

addressStakeKeyHash :: BaseAddress -> Maybe StakeKeyHash
addressStakeKeyHash = addressStakeKeyHashImpl Just Nothing
