module Test.Examples.Config where

import Toppokki (URL)
import Data.Newtype (wrap)

host :: URL
host = wrap "http://localhost:4008/"

namiPassword = "ctltest!"

userDataDir = "./chrome-data"
