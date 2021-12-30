module TestM where

import Prelude
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Array (sort, filter)
import Data.Const (Const)
import Data.Either (Either(..)) 
import Data.Maybe (Maybe(..))
import Data.String (length) as String
import Data.String.CodeUnits (charAt) as String
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Mote (MoteT)
import Node.FS.Aff (readdir)
import Node.Path (FilePath, extname)
import Data.Medea.Loader (LoaderError(..))

appendPath :: FilePath -> FilePath -> FilePath
appendPath a b = case a of
  "" -> b
  str -> case String.charAt 0 b of
    Nothing -> a
    Just h ->
      if h == '/' then
        a <> b
      else do
        let
          i = String.length a - 1
        case String.charAt i a of
          Just '/' -> a <> b
          _ -> a <> "/" <> b

infixr 4 appendPath as </>

listMedeaFiles :: FilePath -> Aff (Array FilePath)
listMedeaFiles dirpath = do
  contents <- readdir dirpath
  pure $ map (dirpath </> _) $ sort $ filter ((_ == ".medea") <<< extname) $ contents

newtype TestM a
  = TestM (ExceptT LoaderError Aff a)

derive newtype instance functorTestM :: Functor TestM

derive newtype instance applyTestM :: Apply TestM

derive newtype instance applicativeTestM :: Applicative TestM

derive newtype instance bindTestM :: Bind TestM

derive newtype instance monadTestM :: Monad TestM

derive newtype instance monadThrowTestM :: MonadThrow LoaderError TestM

derive newtype instance monadErrorTestM :: MonadError LoaderError TestM

derive newtype instance monadEffectTestM :: MonadEffect TestM

derive newtype instance monadAffTestM :: MonadAff TestM

type TestPlanM a
  = MoteT (Const Void) (Aff Unit) Aff Unit

runTestM :: forall a. TestM a -> Aff (Either LoaderError a)
runTestM (TestM comp) = runExceptT comp

isParseError :: forall a. Either LoaderError a -> Boolean
isParseError (Left NotUtf8) = true

isParseError (Left IdentifierTooLong) = true

isParseError (Left (ParserError _)) = true

isParseError _ = false

isSchemaError :: forall a. Either LoaderError a -> Boolean
isSchemaError (Left StartSchemaMissing) = true

isSchemaError (Left SelfTypingSchema) = true

isSchemaError (Left (MultipleSchemaDefinition _)) = true

isSchemaError (Left (MissingSchemaDefinition _ _)) = true

isSchemaError (Left (SchemaNameReserved _)) = true

isSchemaError (Left (IsolatedSchemata _)) = true

isSchemaError (Left (MissingPropSchemaDefinition _ _)) = true

isSchemaError (Left (MinimumLengthGreaterThanMaximum _)) = true

isSchemaError (Left (MultiplePropSchemaDefinition _ _)) = true

isSchemaError (Left (MissingListSchemaDefinition _ _)) = true

isSchemaError (Left (MissingTupleSchemaDefinition _ _)) = true

isSchemaError (Left (PropertySpecWithoutObjectType _)) = true

isSchemaError (Left (ListSpecWithoutArrayType _)) = true

isSchemaError (Left (TupleSpecWithoutArrayType _)) = true

isSchemaError (Left (StringSpecWithoutStringType _)) = true

isSchemaError _ = false
