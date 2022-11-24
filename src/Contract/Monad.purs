-- | A module defining the `Contract` monad.
module Contract.Monad
  ( module ExportAff
  , module ExportContract
  , module ExportLogTag
  , liftContractAffM
  , liftContractE
  , liftContractE'
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , throwContractError
  ) where

import Prelude

import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Contract.Monad
  ( Contract(Contract)
  , ContractEnv
  , ContractParams
  , mkContractEnv
  , runContract
  , runContractInEnv
  , stopContractEnv
  , withContractEnv
  ) as ExportContract
import Data.Either (Either, either, hush)
import Data.Log.Tag
  ( TagSet
  , booleanTag
  , intTag
  , jsDateTag
  , numberTag
  , tag
  , tagSetTag
  ) as ExportLogTag
import Data.Maybe (Maybe, maybe)
import Effect.Aff (Aff)
import Effect.Aff (Aff, launchAff_) as ExportAff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

-- | Same as `liftContractM` but the `Maybe` value is in the `Aff` context.
liftContractAffM :: forall (a :: Type). String -> Aff (Maybe a) -> Contract a
liftContractAffM str = liftedM str <<< liftAff

-- | Similar to `liftContractE` except it directly throws the showable error
-- | via `throwContractError` instead of an arbitrary string.
liftContractE
  :: forall (e :: Type) (a :: Type). Show e => Either e a -> Contract a
liftContractE = either throwContractError pure

-- | Similar to `liftContractM`, throwing the string instead of the `Left`
-- | value. For throwing the `Left` value, see `liftEither` in
-- | `Contract.Prelude`.
liftContractE'
  :: forall (e :: Type) (a :: Type). String -> Either e a -> Contract a
liftContractE' str = liftContractM str <<< hush

-- | Given a string error and `Maybe` value, if the latter is `Nothing`, throw
-- | the error with the given string, otherwise, return the value. If using
-- | using `runExceptT`, see `liftM` inside `Contract.Prelude`. This can be
-- | thought of as `liftM` restricted to JavaScript's `Error` and without the
-- | need to call `error :: String -> Error` each time.
liftContractM :: forall (a :: Type). String -> Maybe a -> Contract a
liftContractM str = maybe (liftEffect $ throw str) pure

-- | Similar to `liftedE` except it directly throws the showable error via
-- | `throwContractError` instead of an arbitrary string.
liftedE
  :: forall (e :: Type) (a :: Type)
   . Show e
  => Contract (Either e a)
  -> Contract a
liftedE = (=<<) liftContractE

-- | Same as `liftContractE` but the `Either` value is already in the `Contract`
-- | context.
liftedE'
  :: forall (e :: Type) (a :: Type)
   . String
  -> Contract (Either e a)
  -> Contract a
liftedE' str = (=<<) (liftContractE' str)

-- | Same as `liftContractM` but the `Maybe` value is already in the `Contract`
-- | context.
liftedM :: forall (a :: Type). String -> Contract (Maybe a) -> Contract a
liftedM str = (=<<) (liftContractM str)

-- | Throws an `Error` for any showable error using `Effect.Exception.throw`
-- | and lifting into the `Contract` monad.
throwContractError :: forall (e :: Type) (a :: Type). Show e => e -> Contract a
throwContractError = liftEffect <<< throw <<< show

