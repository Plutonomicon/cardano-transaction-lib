module Test.Ctl.ApplyArgs (main, suite, contract) where

import Contract.Prelude

import Contract.Monad (Contract, launchAff_)
import Contract.PlutusData (PlutusData(List, Map, Bytes, Constr), toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Scripts (PlutusScript)
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV1FromEnvelope
  , plutusScriptV2FromEnvelope
  )
import Control.Monad.Error.Class (class MonadError)
import Ctl.Internal.ApplyArgs (applyArgs)
import Ctl.Internal.Cardano.TextEnvelope (TextEnvelope)
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.BigInt (fromInt)
import Data.List.Lazy (replicate)
import Data.Profunctor.Choice (left)
import Effect.Aff (Error, error, throwError)
import Foreign.Object (Object)
import Foreign.Object as Object
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)

foreign import scripts :: Object String

main :: Effect Unit
main = launchAff_ $ interpret $ suite

contract :: Contract () Unit
contract = do
  traverse_ (uncurry $ compareApplied v1) $ Tuple <$> v1ScriptPaths <*> params
  traverse_ (uncurry $ compareApplied v2) $ Tuple <$> v2ScriptPaths <*> params

suite :: TestPlanM (Aff Unit) Unit
suite = group "Applying params to scripts test" $ do
  traverse_ (uncurry $ testCase v1) $ Tuple <$> v1ScriptPaths <*> params
  traverse_ (uncurry $ testCase v2) $ Tuple <$> v2ScriptPaths <*> params

  where

  testCase lang scriptName (args /\ argsName) =
    test
      ("Apply " <> argsName <> " to " <> scriptName)
      $ compareApplied lang scriptName (args /\ argsName)

compareApplied
  :: forall (m :: Type -> Type)
   . Monad m
  => MonadError Error m
  => (String -> m PlutusScript)
  -> String
  -> Tuple (Array PlutusData) String
  -> m Unit
compareApplied lang scriptName (args /\ argsName) = do
  script <- lang scriptName
  applied <- liftEither $ left (error <<< show) $ applyArgs script args
  appliedShouldBe <- lang (scriptName <> "-" <> argsName)
  applied `shouldEqual` appliedShouldBe

v1ScriptPaths :: Array String
v1ScriptPaths =
  [ "always-fails"
  , "include-datum"
  , "one-shot-minting"
  , "redeemer1-validator"
  ]

v2ScriptPaths :: Array String
v2ScriptPaths =
  [ "always-succeeds-v2"
  , "one-shot-minting-v2"
  , "check-datum-is-inline"
  ]

params :: Array (Tuple (Array PlutusData) String)
params =
  [ ([] /\ "no-args")
  , ([ un ] /\ "unit")
  , ( [ i 7
      , un
      , List [ un, bytes ]
      , longBytes
      , Map [ (i 5 /\ i 7), (bytes /\ i 8) ]
      , Constr (fromInt 102) [ i 7, List [ un, bytes, longBytes ] ]
      , Constr (fromInt 5)
          [ List []
          , List [ i 1 ]
          , Map []
          , Map [ (i 1 /\ un), (i 2 /\ Constr (fromInt 2) [ i 2 ]) ]
          ]
      ] /\ "big-arg"
    )
  ]

i :: Int -> PlutusData
i k = toData (fromInt k)

un :: PlutusData
un = toData unit

bytes :: PlutusData
bytes = Bytes $ hexToByteArrayUnsafe "4d5f"

longBytes :: PlutusData
longBytes = Bytes $ hexToByteArrayUnsafe $ foldl (\x y -> x <> y) "" $
  replicate 65 "4d"

v1 :: forall (m :: Type -> Type). MonadError Error m => String -> m PlutusScript
v1 = lookupAux plutusScriptV1FromEnvelope

v2 :: forall (m :: Type -> Type). MonadError Error m => String -> m PlutusScript
v2 = lookupAux plutusScriptV2FromEnvelope

lookupAux
  :: forall (m :: Type -> Type)
   . MonadError Error m
  => (TextEnvelope -> Maybe PlutusScript)
  -> String
  -> m PlutusScript
lookupAux decodeScript name =
  maybe (throwError $ error $ "Can't find the script with name " <> name) pure
    $ do
        txt <- Object.lookup name scripts
        envelope <- decodeTextEnvelope txt
        decodeScript envelope
