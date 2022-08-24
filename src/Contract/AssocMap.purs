-- | A module for a Plutus-style `AssocMap`
module Contract.AssocMap (module AssocMap) where

import Plutus.Types.AssocMap
  ( Map(Map)
  , delete
  , elems
  , empty
  , filter
  , insert
  , keys
  , lookup
  , mapMaybe
  , mapMaybeWithKey
  , mapThese
  , mapWithKey
  , member
  , null
  , singleton
  , union
  , unionWith
  , values
  ) as AssocMap
