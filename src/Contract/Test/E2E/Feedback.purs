-- | Store and retrieve values in simple JS variable (window.ctlTestFeedback),
-- | in order to establish a communication between the Examples and E2E tests.
-- | Retrieval must be called from Puppeteer, while publishing happens in the browser.
module Contract.Test.E2E.Feedback
  ( BrowserEvent
      ( ConfirmAccess
      , Sign
      , Success
      , Failure
      , Message
      )
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  , encodeAeson'
  , (.:)
  )
import Control.Alt ((<|>))
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Effect (Effect)

data BrowserEvent a
  = ConfirmAccess
  | Sign
  | Success
  | Failure String
  | Message a

derive instance Generic a rep => Generic (BrowserEvent a) _

instance EncodeAeson a => EncodeAeson (BrowserEvent a) where
  encodeAeson' ConfirmAccess = encodeAeson' "ConfirmAccess"
  encodeAeson' Sign = encodeAeson' "Sign"
  encodeAeson' Success = encodeAeson' "Success"
  encodeAeson' (Failure str) = encodeAeson' { tag: "Failure", error: str }
  encodeAeson' (Message src) = encodeAeson'
    { tag: "Message", value: encodeAeson src }

instance DecodeAeson a => DecodeAeson (BrowserEvent a) where
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
        "Message" -> do
          Message <$> obj .: "value"
        _ -> Left (TypeMismatch "BrowserEvent")

foreign import _publishTestFeedback :: forall (a :: Type). a -> Effect Unit
