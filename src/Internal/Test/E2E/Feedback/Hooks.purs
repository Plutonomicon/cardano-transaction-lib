module Ctl.Internal.Test.E2E.Feedback.Hooks where

import Prelude

import Ctl.Internal.QueryM (Hooks)
import Ctl.Internal.Test.E2E.Feedback
  ( BrowserEvent(Sign, ConfirmAccess, Success, Failure)
  )
import Ctl.Internal.Test.E2E.Feedback.Browser (pushBrowserEvent)
import Data.Maybe (Maybe(Just))

e2eFeedbackHooks :: Hooks
e2eFeedbackHooks =
  { beforeSign: Just $ pushBrowserEvent Sign
  , beforeInit: Just $ pushBrowserEvent ConfirmAccess
  , onSuccess: Just $ pushBrowserEvent Success
  , onError: Just (pushBrowserEvent <<< Failure <<< show)
  }
