module Test.Ctl.Types.Transaction
  ( suite
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeJsonString
  , printJsonDecodeError
  )
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  )
import Control.Monad.Error.Class (liftEither)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Bifunctor (lmap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Mote (group)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.Path (concat) as Path
import Test.Ctl.Utils (toFromAesonTest)

loadAesonFixture
  :: forall (a :: Type). DecodeAeson a => String -> Effect a
loadAesonFixture filename = do
  contents <- readTextFile UTF8 path
  liftEither $ lmap
    (error <<< ((path <> "\n  ") <> _) <<< printJsonDecodeError)
    (decodeJsonString contents)
  where
  path :: String
  path = Path.concat
    [ "fixtures", "test", "aeson", filename <> ".json" ]

transactionInputFixture :: Effect TransactionInput
transactionInputFixture = loadAesonFixture "TransactionInput"

transactionOutputWithRefScriptFixture :: Effect TransactionOutputWithRefScript
transactionOutputWithRefScriptFixture = loadAesonFixture
  "TransactionOutputWithRefScript"

toFromAesonTest'
  :: forall a
   . Eq a
  => DecodeAeson a
  => EncodeAeson a
  => Show a
  => String
  -> Effect a
  -> TestPlanM (Aff Unit) Unit
toFromAesonTest' msg a = do
  a' <- liftEffect a
  toFromAesonTest msg a'

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Transaction-related Aeson representation tests" do
    toFromAesonTest' "TransactionInput" transactionInputFixture
    toFromAesonTest' "TransactionOutputWithRefScript"
      transactionOutputWithRefScriptFixture
