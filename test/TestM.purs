module TestM where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Medea (ValidationError(EmptyError))
import Effect.Aff (Aff)
import Mote (MoteT)

type TestPlanM a = MoteT (Const Void) (Aff Unit) Aff a

-- this silly thing is needed because Medea's `validate` needs both
-- MonadPlus and MonadError, there must be a better way
-- or it should be upstreamed to medea-ps as a default
newtype ValidationM a = ValidationM (ExceptT ValidationError Identity a)

derive newtype instance functorValidationM :: Functor ValidationM
derive newtype instance applyValidationM :: Apply ValidationM
derive newtype instance applicativeValidationM :: Applicative ValidationM
derive newtype instance bindValidationM :: Bind ValidationM
derive newtype instance monadValidationM :: Monad ValidationM
derive newtype instance monadThrowValidationM ::
  MonadThrow ValidationError ValidationM

derive newtype instance monadErrorValidationM ::
  MonadError ValidationError ValidationM

-- note: MonadZero is being deprecated
derive newtype instance monadZeroValidationM :: MonadZero ValidationM
derive newtype instance monadPlusValidationM :: MonadPlus ValidationM
instance altValidationM :: Alt ValidationM where
  alt (ValidationM first) (ValidationM second) = case runExceptT first of
    (Identity (Right a)) -> pure a
    (Identity (Left _)) -> case runExceptT second of
      (Identity (Right a)) -> pure a
      (Identity (Left e)) -> throwError e

instance plusValidationM :: Plus ValidationM where
  empty = throwError EmptyError

instance alternativeValidationM :: Alternative ValidationM

runValidationM :: forall a. ValidationM a -> Either ValidationError a
runValidationM (ValidationM etvia) = do
  let (Identity eva) = runExceptT etvia
  eva
