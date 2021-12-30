module Test.Parser where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Medea.Loader (LoaderError, loadSchemaFromFile)
import Data.Medea.Schema (Schema)
import Test.Spec.Assertions (shouldSatisfy, shouldNotSatisfy)
import TestM (TestPlanM)
import Mote (group, test)

suite :: TestPlanM Unit
suite = pure unit
