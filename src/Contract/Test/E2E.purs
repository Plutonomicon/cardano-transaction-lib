module Contract.Test.E2E
  ( module Browser
  , module Helpers
  , module WalletExt
  ) where

import Contract.Test.E2E.Browser
  ( Mode(Headless, Visible)
  , TestOptions
  , withBrowser
  , parseOptions
  ) as Browser

-- import Contract.Test.E2E.Feedback
--   ( ) as Feedback

import Contract.Test.E2E.Helpers
  ( WalletPassword
  , delaySec
  , flintConfirmAccess
  , flintSign
  , geroConfirmAccess
  , geroSign
  , namiConfirmAccess
  , namiSign
  , withExample
  ) as Helpers

import Contract.Test.E2E.Types
  ( SomeWallet
  , WalletExt(FlintExt, GeroExt, NamiExt, LodeExt, EternlExt)
  ) as WalletExt
