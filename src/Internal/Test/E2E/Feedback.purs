-- | Store and retrieve values in simple JS variable (window.ctlTestFeedback),
-- | in order to establish a communication between the Examples and E2E tests.
-- | Retrieval must be called from Puppeteer, while publishing happens in the browser.
module Ctl.Internal.Test.E2E.Feedback
  ( BrowserEvent
      ( ConfirmAccess
      , Sign
      , Success
      , Failure
      )
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson'
  , (.:)
  )
import Control.Alt ((<|>))
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)

data BrowserEvent
  = ConfirmAccess
  | Sign
  | Success
  | Failure String

derive instance Generic BrowserEvent _

instance Show BrowserEvent where
  show = genericShow

instance EncodeAeson BrowserEvent where
  encodeAeson' ConfirmAccess = encodeAeson' "ConfirmAccess"
  encodeAeson' Sign = encodeAeson' "Sign"
  encodeAeson' Success = encodeAeson' "Success"
  encodeAeson' (Failure str) = encodeAeson' { tag: "Failure", error: str }

instance DecodeAeson BrowserEvent where
  decodeAeson aeson =
    decodeObject <|> decodeString
    where
    decodeString = do
      decodeAeson aeson >>= case _ of
        "ConfirmAccess" -> pure ConfirmAccess
        "Sign" -> pure Sign
        "Success" -> pure Success
        _ -> Left (TypeMismatch "BrowserEvent")
    decodeObject = do
      obj <- decodeAeson aeson
      tag <- obj .: "tag"
      case tag of
        "Failure" -> do
          Failure <$> obj .: "error"
        _ -> Left (TypeMismatch "BrowserEvent")

foreign import _publishTestFeedback :: forall (a :: Type). a -> Effect Unit
