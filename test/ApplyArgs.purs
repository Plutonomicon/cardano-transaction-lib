module Test.Ctl.ApplyArgs (main, suite, contract) where

import Contract.Prelude

import Contract.Monad (Contract, launchAff_)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (PlutusData(List, Map, Bytes, Constr), toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Scripts (PlutusScript)
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV1FromEnvelope
  , plutusScriptV2FromEnvelope
  )
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Ctl.Examples.Helpers.LoadScript (loadScript)
import Ctl.Internal.ApplyArgs (applyArgs)
import Ctl.Internal.Cardano.TextEnvelope (TextEnvelope)
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.BigInt (fromInt)
import Data.List.Lazy (replicate)
import Data.Profunctor.Choice (left)
import Effect.Aff (Error, error, throwError)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)

scriptFilenames :: Object String
scriptFilenames =
  Object.fromFoldable
    [ "always-fails" /\ "always-fails.plutus"
    , "include-datum" /\ "include-datum.plutus"
    , "one-shot-minting" /\ "one-shot-minting.plutus"
    , "redeemer1-validator" /\ "redeemer1-validator.plutus"
    , "always-succeeds-v2" /\ "always-succeeds-v2.plutus"
    , "one-shot-minting-v2" /\ "one-shot-minting-v2.plutus"
    , "check-datum-is-inline" /\ "check-datum-is-inline.plutus"
    , "always-fails-big-arg" /\ "applied/always-fails-big-arg.plutus"
    , "always-fails-no-args" /\ "applied/always-fails-no-args.plutus"
    , "always-fails-unit" /\ "applied/always-fails-unit.plutus"
    , "always-succeeds-v2-big-arg" /\
        "applied/always-succeeds-v2-big-arg.plutus"
    , "always-succeeds-v2-no-args" /\
        "applied/always-succeeds-v2-no-args.plutus"
    , "always-succeeds-v2-unit" /\ "applied/always-succeeds-v2-unit.plutus"
    , "check-datum-is-inline-big-arg" /\
        "applied/check-datum-is-inline-big-arg.plutus"
    , "check-datum-is-inline-no-args" /\
        "applied/check-datum-is-inline-no-args.plutus"
    , "check-datum-is-inline-unit" /\
        "applied/check-datum-is-inline-unit.plutus"
    , "include-datum-big-arg" /\ "applied/include-datum-big-arg.plutus"
    , "include-datum-no-args" /\ "applied/include-datum-no-args.plutus"
    , "include-datum-unit" /\ "applied/include-datum-unit.plutus"
    , "one-shot-minting-big-arg" /\ "applied/one-shot-minting-big-arg.plutus"
    , "one-shot-minting-no-args" /\ "applied/one-shot-minting-no-args.plutus"
    , "one-shot-minting-unit" /\ "applied/one-shot-minting-unit.plutus"
    , "one-shot-minting-v2-big-arg" /\
        "applied/one-shot-minting-v2-big-arg.plutus"
    , "one-shot-minting-v2-no-args" /\
        "applied/one-shot-minting-v2-no-args.plutus"
    , "one-shot-minting-v2-unit" /\ "applied/one-shot-minting-v2-unit.plutus"
    , "redeemer1-validator-big-arg" /\
        "applied/redeemer1-validator-big-arg.plutus"
    , "redeemer1-validator-no-args" /\
        "applied/redeemer1-validator-no-args.plutus"
    , "redeemer1-validator-unit" /\ "applied/redeemer1-validator-unit.plutus"
    ]

main :: Effect Unit
main = launchAff_ $ interpret $ suite

contract :: Contract Unit
contract = do
  scripts <- liftAff $ parTraverse loadScript scriptFilenames
  traverse_ (uncurry $ compareApplied (v1 scripts)) $ Tuple <$> v1ScriptPaths
    <*> params
  traverse_ (uncurry $ compareApplied (v2 scripts)) $ Tuple <$> v2ScriptPaths
    <*> params

suite :: TestPlanM (Aff Unit) Unit
suite = group "Applying params to scripts test" $ do
  scripts <- lift $ parTraverse loadScript scriptFilenames
  traverse_ (uncurry $ testCase (v1 scripts)) $ Tuple <$> v1ScriptPaths <*>
    params
  traverse_ (uncurry $ testCase (v2 scripts)) $ Tuple <$> v2ScriptPaths <*>
    params
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
  , "check-datum-is-inline$"
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
      , Constr (BigNum.fromInt 102) [ i 7, List [ un, bytes, longBytes ] ]
      , Constr (BigNum.fromInt 5)
          [ List []
          , List [ i 1 ]
          , Map []
          , Map [ (i 1 /\ un), (i 2 /\ Constr (BigNum.fromInt 2) [ i 2 ]) ]
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

v1
  :: forall (m :: Type -> Type)
   . MonadError Error m
  => Object String
  -> String
  -> m PlutusScript
v1 scripts name = lookupAux plutusScriptV1FromEnvelope scripts name

v2
  :: forall (m :: Type -> Type)
   . MonadError Error m
  => Object String
  -> String
  -> m PlutusScript
v2 scripts name = lookupAux plutusScriptV2FromEnvelope scripts name

lookupAux
  :: forall (m :: Type -> Type)
   . MonadError Error m
  => (TextEnvelope -> Maybe PlutusScript)
  -> Object String
  -> String
  -> m PlutusScript
lookupAux decodeScript scripts name =
  maybe (throwError $ error $ "Can't find the script with name " <> name) pure
    $ do
        txt <- Object.lookup name scripts
        envelope <- decodeTextEnvelope txt
        decodeScript envelope
