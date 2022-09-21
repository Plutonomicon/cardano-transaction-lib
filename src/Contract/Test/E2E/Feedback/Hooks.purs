module Contract.Test.E2E.Feedback.Hooks where

import Prelude

import Contract.Test.E2E.Feedback
  ( BrowserEvent(Sign, ConfirmAccess, Success, Failure)
  )
import Contract.Test.E2E.Feedback.Browser (pushBrowserEvent)
import Data.Maybe (Maybe(Just))
import QueryM (Hooks)

feedbackHooks :: Hooks
feedbackHooks =
  { beforeSign: Just $ pushBrowserEvent (Sign :: BrowserEvent {})
  , beforeInit: Just $ pushBrowserEvent (ConfirmAccess :: BrowserEvent {})
  , onSuccess: Just $ pushBrowserEvent (Success :: BrowserEvent {})
  , onError: Just \err -> pushBrowserEvent
      (Failure $ show err :: BrowserEvent {})
  }
