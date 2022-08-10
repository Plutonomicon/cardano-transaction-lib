module Utils (bshow) where

import Data.ByteString.Char8 qualified as C8
import Data.Kind (Type)

bshow :: forall (a :: Type). Show a => a -> C8.ByteString
bshow = C8.pack . show
