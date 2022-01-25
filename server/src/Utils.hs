module Utils (bshow,lbshow) where

import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy.Char8 qualified as LC8

bshow :: Show a => a -> C8.ByteString
bshow = C8.pack . show

lbshow :: Show a => a -> LC8.ByteString
lbshow = LC8.fromStrict . bshow
