module Error
  ( E
  , NotImplementedError
  , _notImplementedError
  , notImplementedError
  , traceAndHushAll
  , traceAndHushAll_
  , hushable
  , safe_
  , noteE
  , noteEM
  ) where

import Data.Maybe

import Contract.Prelude
  ( class Monad
  , Either(..)
  , pure
  , unwrap
  , (*>)
  , (<<<)
  , (>>=)
  , (>>>)
  )
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Except.Checked (ExceptV, safe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Function (($))
import Data.Identity (Identity)
import Data.Variant (Variant, inj)
import Debug (class DebugWarning, traceM)
import Helpers (notImplemented)
import Prim.Row (class Union)
import Prim.TypeError (class Warn, Text)
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

-- we could use the left side to store logs to get better error tracing
type E v a = Either (Variant v) a

type NotImplementedError r = (notImplementedError :: String | r)
_notImplementedError = Proxy :: Proxy "notImplementedError"

notImplementedError
  :: forall (r :: Row Type) (a :: Type)
   . Warn (Text "Function not implemented!")
  => String
  -> E (NotImplementedError + r) a
notImplementedError = throwError <<< inj _notImplementedError

-- | Allows hush errors tracing as debug outputs.
-- | For use when error hadling is not yet supported at a call site.
traceAndHushAll_
  :: forall (v :: Row Type) (a :: Type)
   . DebugWarning
  => ExceptV v Identity a
  -> Maybe a
traceAndHushAll_ = unwrap <<< runMaybeT <<< traceAndHushAll

-- | Allows hush errors tracing as debug outputs.
-- | For use when error hadling is not yet supported at a call site.
traceAndHushAll
  :: forall (v :: Row Type) (m :: Type -> Type) (a :: Type)
   . DebugWarning
  => Monad m
  => ExceptV v m a
  -> MaybeT m a
traceAndHushAll act = MaybeT $ runExceptT act >>= case _ of
  Left v -> traceM v *> pure Nothing
  Right a -> pure (pure a)

-- | Lifts underlying monad to MaybeT allowing hushing selected errors.
hushable
  :: forall (v :: Row Type) (m :: Type -> Type) (a :: Type)
   . Monad m
  => ExceptV v m a
  -> ExceptV v (MaybeT m) a
hushable = runExceptT >>> lift >>> ExceptT

-- TODO traceAndHushOne
-- traceAndHushOne p = handleError (Record.set p.proxy (\x -> traceM p.name *> traceM x *> lift (MaybeT $ pure Nothing)) {})

safe_
  :: forall (a :: Type)
   . ExceptV () Identity a
  -> a
safe_ = safe >>> unwrap

-- | Annotates Maybe with more proper error instead of the Nothing case
noteE :: forall v a. E v a -> Maybe a -> E v a
noteE e = maybe e Right

-- TODO
noteEM
  :: forall v1 v2 v3 a m
   . Union v1 v2 v3
  => Monad m
  => ExceptV v1 m a
  -> ExceptV v2 m (Maybe a)
  -> ExceptV v3 m a
noteEM _ _ = notImplemented

-- inj
--   ∷ ∀ proxy sym a r1 r2
--   . R.Cons sym a r1 r2
--   ⇒ IsSymbol sym
--   ⇒ proxy sym
--   → a
--   → Variant r2
-- inj p value = coerceV $ VariantRep { type: reflectSymbol p, value }

-- wrapErr
--   :: forall a r
--    . String
--   -> E (WrappedError + r) a
--   -> E (WrappedError + WrappedError + r) a
-- wrapErr str = lmap (\e -> inj _wrappedError.proxy (Tuple str e))
