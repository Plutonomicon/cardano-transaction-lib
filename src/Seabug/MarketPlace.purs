module Seabug.MarketPlace
  ( marketplaceValidator
  )
  where

import Contract.Prelude
import Contract.Prim.Any (Any)
import Contract.Scripts (TypedValidator)

-- Will likely be read in from JSON
marketplaceValidator :: TypedValidator Any
marketplaceValidator = undefined