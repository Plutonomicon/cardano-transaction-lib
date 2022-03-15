module Contract.Types
  ( Contract
  )
  where

import Prelude
import QueryM (QueryM)

newtype Contract (a :: Type) = Contract (QueryM a)

derive newtype instance Functor Contract