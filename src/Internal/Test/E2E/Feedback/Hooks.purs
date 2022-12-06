module Ctl.Internal.Test.E2E.Feedback.Hooks
  ( e2eFeedbackHooks
  , addE2EFeedbackHooks
  ) where

import Prelude

import Ctl.Internal.Contract.Hooks (Hooks)
import Ctl.Internal.Test.E2E.Feedback
  ( BrowserEvent(Sign, ConfirmAccess, Success, Failure)
  )
import Ctl.Internal.Test.E2E.Feedback.Browser (pushBrowserEvent)
import Data.Maybe (Maybe(Just))
import Data.Traversable (for_)

e2eFeedbackHooks :: Hooks
e2eFeedbackHooks =
  { beforeSign: Just $ pushBrowserEvent Sign
  , beforeInit: Just $ pushBrowserEvent ConfirmAccess
  , onSuccess: Just $ pushBrowserEvent Success
  , onError: Just (pushBrowserEvent <<< Failure <<< show)
  }

addE2EFeedbackHooks :: Hooks -> Hooks
addE2EFeedbackHooks hooks = hooks
  { beforeSign = Just do
      pushBrowserEvent Sign
      for_ hooks.beforeSign identity
  , beforeInit = Just do
      pushBrowserEvent ConfirmAccess
      for_ hooks.beforeInit identity
  , onSuccess = Just do
      pushBrowserEvent Success
      for_ hooks.onSuccess identity
  , onError = Just \error -> do
      pushBrowserEvent <<< Failure $ show error
      for_ hooks.onError \f -> f error
  }
