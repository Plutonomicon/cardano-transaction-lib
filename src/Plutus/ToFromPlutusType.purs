module Plutus.ToFromPlutusType
  ( class FromPlutusType
  , fromPlutusType
  , class ToPlutusType
  , toPlutusType
  ) where

class FromPlutusType :: (Type -> Type) -> Type -> Type -> Constraint
class FromPlutusType f pt t | pt -> t, t pt -> f where
  fromPlutusType :: pt -> f t

class ToPlutusType :: (Type -> Type) -> Type -> Type -> Constraint
class ToPlutusType f t pt | t -> pt, t pt -> f where
  toPlutusType :: t -> f pt
