-- | A custom reporter that will print the entire stack trace of errors from
-- | failed tests. Should be used with `Test.Spec.Runner.runSpec'`.
module Contract.Test.Mote.ConsoleReporter (module X) where

import Ctl.Internal.Test.ConsoleReporter (consoleReporter) as X
