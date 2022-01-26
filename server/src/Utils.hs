module Utils (bshow, lbshow, tshow) where

import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Text (Text)
import Data.Text qualified as Text

bshow :: Show a => a -> C8.ByteString
bshow = C8.pack . show

lbshow :: Show a => a -> LC8.ByteString
lbshow = LC8.fromStrict . bshow

tshow :: Show a => a -> Text
tshow = Text.pack . show
