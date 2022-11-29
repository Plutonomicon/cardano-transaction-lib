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
  , mkContractEnv
  , stopContractEnv
  ) where

import Prelude

import Ctl.Internal.Contract.Monad (Contract, ContractEnv, ContractParams)
import Ctl.Internal.Contract.Monad
  ( Contract(Contract)
  , ContractEnv
  , ContractParams
  , runContract
  , runContractInEnv
  , withContractEnv
  ) as ExportContract
import Ctl.Internal.Contract.Monad
  ( mkContractEnv
  , stopContractEnv
  ) as Contract
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
import Prim.TypeError (class Warn, Text)

-- | Initializes a `Contract` environment. Does not ensure finalization.
-- | Consider using `withContractEnv` if possible - otherwise use
-- | `stopContractEnv` to properly finalize.
mkContractEnv
  :: Warn
       ( Text
           "Using `mkContractEnv` is not recommended: it does not ensure `ContractEnv` finalization. Consider using `withContractEnv`"
       )
  => ContractParams
  -> Aff ContractEnv
mkContractEnv = Contract.mkContractEnv

-- | Finalizes a `Contract` environment.
-- | Closes the connections in `ContractEnv`, effectively making it unusable.
stopContractEnv
  :: Warn
       ( Text
           "Using `stopContractEnv` is not recommended: users should rely on `withContractEnv` to finalize the runtime environment instead"
       )
  => ContractEnv
  -> Aff Unit
stopContractEnv = Contract.stopContractEnv

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

