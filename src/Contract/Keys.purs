-- | Exposes constructors for `PublicKey` and `Ed25519Signature` types
module Contract.Keys
  ( module X
  ) where

import Ctl.Internal.Cardano.Types.Transaction
  ( mkEd25519Signature
  , mkPublicKey
  ) as X
