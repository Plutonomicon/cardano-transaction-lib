module Ctl.Internal.Types.TxBalancer
  ( TxBalancer
  ) where

import Cardano.Types (Transaction)
import Data.Either (Either)

type TxBalancer (m :: Type -> Type) (err :: Type) (ctx :: Type) =
  Transaction
  -> ctx
  -> m (Either err Transaction)
