module Ctl.Internal.Helpers
  ( (</>)
  , (<<>>)
  , (<\>)
  , (<</>>)
  , (??)
  , appendFirstMaybe
  , appendLastMaybe
  , appendMap
  , appendRightMap
  , bigIntToUInt
  , bugTrackerLink
  , concatPaths
  , contentsProp
  , encodeMap
  , encodeTagged
  , encodeTagged'
  , filterMapM
  , filterMapWithKeyM
  , fromJustEff
  , fromMaybeFlipped
  , fromRightEff
  , liftEither
  , liftM
  , liftMWith
  , liftedM
  , logString
  , logWithLevel
  , maybeArrayMerge
  , mkErrorRecord
  , notImplemented
  , showWithParens
  , tagProp
  , uIntToBigInt
  , pprintTagSet
  ) where

import Prelude

import Aeson (class EncodeAeson, Aeson, encodeAeson, toString)
import Control.Monad.Error.Class (class MonadError, throwError)
import Ctl.Internal.Helpers.Formatter (showTags)
import Data.Array (union)
import Data.Bifunctor (bimap)
import Data.Bitraversable (ltraverse)
import Data.Either (Either(Right), either)
import Data.Function (on)
import Data.JSDate (now)
import Data.List.Lazy as LL
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Log.Tag (TagSet)
import Data.Map (Map, toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, maybe)
import Data.Maybe.First (First(First))
import Data.Maybe.Last (Last(Last))
import Data.String (Pattern(Pattern), null, stripPrefix, stripSuffix)
import Data.Traversable (traverse)
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Foreign.Object as Obj
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Partial.Unsafe (unsafePartial)
import Prim.TypeError (class Warn, Text)

bugTrackerLink :: String
bugTrackerLink =
  "https://github.com/Plutonomicon/cardano-transaction-lib/issues"

pprintTagSet :: String -> TagSet -> String
pprintTagSet message tags =
  message <> " " <> showTags tags

-- | Throws provided error on `Nothing`
fromJustEff :: forall (a :: Type). String -> Maybe a -> Effect a
fromJustEff e = case _ of
  Nothing -> throw e
  Just x -> pure x

liftEither
  :: forall (a :: Type) (e :: Type) (m :: Type -> Type)
   . MonadError e m
  => Either e a
  -> m a
liftEither = either throwError pure

fromRightEff :: forall (a :: Type) (e :: Type). Show e => Either e a -> Effect a
fromRightEff = either (throw <<< show) pure

-- | Given an error and a lifted `Maybe` value.
liftedM
  :: forall (e :: Type) (m :: Type -> Type) (a :: Type)
   . MonadError e m
  => e
  -> m (Maybe a)
  -> m a
liftedM err mma = mma >>= maybe (throwError err) Right >>> liftEither

-- | Given an error and a `Maybe` value, lift the context via `liftEither`.
liftM
  :: forall (e :: Type) (m :: Type -> Type) (a :: Type)
   . MonadError e m
  => e
  -> Maybe a
  -> m a
liftM err = liftEither <<< maybe (throwError err) Right

-- | Given an error and a `Maybe` value, lift the context via `liftEither` with
-- | a handler on `Right`.
liftMWith
  :: forall (e :: Type) (m :: Type -> Type) (a :: Type) (b :: Type)
   . MonadError e m
  => e
  -> (a -> b)
  -> Maybe a
  -> m b
liftMWith err f = liftEither <<< maybe (throwError err) (Right <<< f)

-- | Combine two `Maybe`s taking the `First` `Maybe`
appendFirstMaybe :: forall (a :: Type). Maybe a -> Maybe a -> Maybe a
appendFirstMaybe m m' = on (<>) First m m' # \(First m'') -> m''

infixr 5 appendFirstMaybe as <\>

-- | Combine two `Maybe`s taking the `Last` `Maybe`
appendLastMaybe :: forall (a :: Type). Maybe a -> Maybe a -> Maybe a
appendLastMaybe m m' = on (<>) Last m m' # \(Last m'') -> m''

infixr 5 appendLastMaybe as </>

-- | Combine two `Maybe` `Array`'s where `Nothing` and the empty `Array` both
-- | act as an identity
maybeArrayMerge
  :: forall (a :: Type)
   . Eq a
  => Maybe (Array a)
  -> Maybe (Array a)
  -> Maybe (Array a)
maybeArrayMerge Nothing y = y
maybeArrayMerge x Nothing = x
maybeArrayMerge (Just x) (Just y) = Just $ union x y

infixr 5 maybeArrayMerge as <<>>

-- | Provide an append for Maps where the value has as `Semigroup` instance
appendMap
  :: forall (k :: Type) (v :: Type)
   . Ord k
  => Semigroup v
  => Map k v
  -> Map k v
  -> Map k v
appendMap = Map.unionWith (<>)

-- | Provide an append for `Map`s with right bias
appendRightMap
  :: forall (k :: Type) (v :: Type)
   . Ord k
  => Map k v
  -> Map k v
  -> Map k v
appendRightMap = Map.unionWith (flip const)

-- | Filters a map on a Monadic context over a lifted predicate on both the
-- | map's key and value
filterMapWithKeyM
  :: forall (m :: Type -> Type) (k :: Type) (v :: Type)
   . Ord k
  => Monad m
  => (k -> v -> m Boolean)
  -> Map k v
  -> m (Map k v)
filterMapWithKeyM p =
  map Map.fromFoldable <<< LL.filterM (uncurry p) <<< Map.toUnfoldable

-- | Filters a map on a Monadic context over a lifted predicate on the map's
-- | value
filterMapM
  :: forall (m :: Type -> Type) (k :: Type) (v :: Type)
   . Ord k
  => Monad m
  => (v -> m Boolean)
  -> Map k v
  -> m (Map k v)
filterMapM p =
  map Map.fromFoldable <<< LL.filterM (p <<< snd) <<< Map.toUnfoldable

-- UInt.toInt is unsafe so we'll go via String. BigInt.fromString returns a
-- Maybe but we should be safe if we go from UInt originally via String,
-- as this UInt can't be larger than BigInt.
-- | Converts an `UInt` to `BigInt`
uIntToBigInt :: UInt -> BigInt
uIntToBigInt = unsafePartial fromJust <<< BigInt.fromString <<< UInt.toString

-- This should be left allowed to fail as BigInt may exceed UInt
-- | Converts a `BigInt` to `UInt` with potential failure.
bigIntToUInt :: BigInt -> Maybe UInt
bigIntToUInt = UInt.fromString <<< BigInt.toString

notImplemented :: forall a. Warn (Text "Function not implemented!") => a
notImplemented = undefined

-- | Log a message by printing it to the console, depending on the provided
-- | `LogLevel`
logWithLevel
  :: forall (m :: Type -> Type). MonadEffect m => LogLevel -> Message -> m Unit
logWithLevel lvl msg = when (msg.level >= lvl) $ log =<< prettyFormatter msg

-- | Log a message from the JS side of the FFI boundary. The first `LogLevel`
-- | argument represents the configured log level (e.g. within `QueryConfig`).
-- | The second argument is the level for this particular message
logString :: LogLevel -> LogLevel -> String -> Effect Unit
logString cfgLevel level message = do
  timestamp <- now
  logWithLevel cfgLevel { timestamp, message, level, tags: Map.empty }

-- | Used for `EncodeAeson` for datatype errors
mkErrorRecord
  :: forall (a :: Type)
   . String -- Error type
  -> String -- Error
  -> a
  -> { "errorType" :: String
     , "error" :: String
     , "args" :: a
     }
mkErrorRecord errorType error a =
  { "errorType": errorType, "error": error, "args": a }

-- | Provides `Show` instances for Newtypes that do not have inner parenthesis,
-- | e.g. `BigInt`. We could optionally use a `Newtype` constraint for
-- | unwrapping, but we don't constrain ourselves by deconstructing the wrapper.
showWithParens
  :: forall (a :: Type)
   . Show a
  => String
  -> a -- the inner type.
  -> String
showWithParens ctorName x = "(" <> ctorName <> " (" <> show x <> "))"

-- | If `k` is encoded as string, `encodeMap` encodes `Map` as `Object`,
-- | else as an `Array` of `Aeson /\ Aeson` pairs
encodeMap
  :: forall (k :: Type) (v :: Type)
   . EncodeAeson k
  => EncodeAeson v
  => Map k v
  -> Aeson
encodeMap m =
  case traverse (ltraverse toString) pairs of
    Just pairs' -> encodeAeson $ Obj.fromFoldable pairs'
    Nothing -> encodeAeson pairs
  where
  pairs :: Array (Aeson /\ Aeson)
  pairs = map (bimap encodeAeson encodeAeson) $ toUnfoldable m

tagProp :: String
tagProp = "tag"

contentsProp :: String
contentsProp = "contents"

-- | Args: tag value encoder
-- | Encodes `value` using `encoder` as `{ "tag": *encoded tag*, "contents": *encoded value* }`
encodeTagged :: forall a. String -> a -> (a -> Aeson) -> Aeson
encodeTagged tag a encoder =
  encodeAeson $ Obj.fromFoldable
    [ tagProp /\ encodeAeson tag
    , contentsProp /\ encoder a
    ]

-- | A wrapper around `encodeTagged` function that uses
-- | `encodeAeson` for encoding the passed value
encodeTagged' :: forall (a :: Type). EncodeAeson a => String -> a -> Aeson
encodeTagged' str x = encodeTagged str x encodeAeson

-- | Concat two strings with "/" in the middle, but stripping multiple slashes.
-- No slash if second string empty.
concatPaths :: String -> String -> String
concatPaths a b =
  if null right then left
  else left <> "/" <> right

  where
  left = fromMaybe a (stripSuffix (Pattern "/") a)
  right = fromMaybe b (stripPrefix (Pattern "/") b)

infixr 5 concatPaths as <</>> -- </> is taken

fromMaybeFlipped :: forall (a :: Type). Maybe a -> a -> a
fromMaybeFlipped = flip fromMaybe

infixl 5 fromMaybeFlipped as ??
