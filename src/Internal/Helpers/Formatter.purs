-- | Copy of Data.Log.Formatter, but with `showTags` exported and with
-- | MonadEffect use removed (it was only used to format JSDate, the
-- | side-effects of this operation is something we can ignore)
module Ctl.Internal.Helpers.Formatter
  ( showTags
  ) where

import Prelude

import Ansi.Codes (Color(Yellow))
import Ansi.Output (bold, foreground, withGraphics)
import Data.Array (concat, cons, singleton)
import Data.JSDate (JSDate, toISOString)
import Data.Log.Tag
  ( Tag(StringTag, NumberTag, IntTag, BooleanTag, JSDateTag, TagSetTag)
  , TagSet
  )
import Data.Map (isEmpty, toUnfoldable)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(Tuple))
import Effect.Unsafe (unsafePerformEffect)

showTags :: TagSet -> String
showTags = tagLines >>> case _ of
  Nothing -> ""
  Just lines -> append "\n" (joinWith "\n" lines)

tagLines :: TagSet -> Maybe (Array String)
tagLines tags
  | isEmpty tags = Nothing
  | otherwise = Just $ indentEachLine <$> concat (lineify tags)

lineify :: TagSet -> Array (Array String)
lineify tags = showField <$> toUnfoldable tags

showField :: Tuple String Tag -> Array String
showField (Tuple name value) = showTag value $ bold' name <> bold' ": "

showTag :: Tag -> String -> Array String
showTag (StringTag value) = showBasic value
showTag (IntTag value) = showSpecial $ show value
showTag (NumberTag value) = showSpecial $ show value
showTag (BooleanTag value) = showSpecial $ show value
showTag (TagSetTag value) = showSubTags value
showTag (JSDateTag value) = showJsDate value

showSubTags :: TagSet -> String -> Array String
showSubTags value label = cons label $ fromMaybe [] (tagLines value)

showJsDate :: JSDate -> String -> Array String
showJsDate value label =
  showSpecial (unsafePerformEffect (toISOString value)) label

showBasic :: String -> String -> Array String
showBasic value label = singleton $ label <> value

showSpecial :: String -> String -> Array String
showSpecial = color Yellow >>> showBasic

indentEachLine :: String -> String
indentEachLine = append "   "

color :: Color -> String -> String
color = foreground >>> withGraphics

bold' :: String -> String
bold' = withGraphics bold
