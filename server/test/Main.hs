module Main (main) where

import Api (app, applyArgs)
import Data.Kind (Type)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Plutus.V1.Ledger.Api qualified as Ledger
import Servant.Client (
  BaseUrl (baseUrlPort),
  ClientEnv,
  ClientError,
  ClientM,
  mkClientEnv,
  parseBaseUrl,
  runClientM,
 )
import Test.Hspec (
  ActionWith,
  Spec,
  around,
  context,
  describe,
  hspec,
  it,
  runIO,
  shouldBe,
 )
import Test.Hspec.Core.Spec (SpecM)
import Types (
  AppliedScript (AppliedScript),
  ApplyArgsRequest (ApplyArgsRequest, args, script),
  unsafeDecode,
 )

main :: IO ()
main = hspec serverSpec

serverSpec :: Spec
serverSpec = do
  describe "Api.Handlers.applyArgs" applyArgsSpec

applyArgsSpec :: Spec
applyArgsSpec = around withTestApp $ do
  clientEnv <- setupClientEnv

  context "POST apply-args" $ do
    it "returns the same script when called without args" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          applyArgs unappliedRequestFixture
      result `shouldBe` Right (AppliedScript unappliedScript)

    it "returns the correct partially applied Plutus script" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          applyArgs partiallyAppliedRequestFixture
      result `shouldBe` Right (AppliedScript partiallyAppliedScript)

    it "returns the correct fully applied Plutus script" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          applyArgs fullyAppliedRequestFixture
      result `shouldBe` Right (AppliedScript fullyAppliedScript)

setupClientEnv :: SpecM Port (Port -> ClientEnv)
setupClientEnv = do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  pure $
    let clientEnv port = mkClientEnv manager $ baseUrl {baseUrlPort = port}
     in clientEnv

withTestApp :: ActionWith (Port -> IO ())
withTestApp = Warp.testWithApplication $ pure app

runClientM' ::
  forall (a :: Type).
  ClientEnv ->
  ClientM a ->
  IO (Either ClientError a)
runClientM' = flip runClientM

unappliedRequestFixture :: ApplyArgsRequest
unappliedRequestFixture =
  ApplyArgsRequest
    { script = unappliedScript
    , args = []
    }

partiallyAppliedRequestFixture :: ApplyArgsRequest
partiallyAppliedRequestFixture = unappliedRequestFixture {args = [Ledger.I 32]}

fullyAppliedRequestFixture :: ApplyArgsRequest
fullyAppliedRequestFixture =
  unappliedRequestFixture
    { args =
        [ Ledger.I 32
        , Ledger.B "test"
        ]
    }

-- mkTestValidator :: Integer -> CurrencySymbol -> BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mkTestValidator i (CurrencySymbol cs) _ _ _ =
--   if i P.== 1 && cs P.== "" then () else P.error ()

-- mkTestValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mkTestValidatorUntyped p1 p2 =
--   mkTestValidator
--     (unsafeFromBuiltinData p1)
--     (unsafeFromBuiltinData p2)

-- unappliedScript :: Script
-- unappliedScript =
--   fromCompiledCode
--     $$(PlutusTx.compile [||mkTestValidatorUntyped||])
unappliedScript :: Ledger.Script
unappliedScript =
  unsafeDecode
    "Script"
    "\"586f010000333222323233222233222225335300c33225335300e0021001100f333500b\
    \22333573466e1c00800404003c0152002333500b22333573466e3c00800404003c0112201\
    \0010091326353008009498cd4015d680119a802bae0011200120011200112001122002122\
    \001200101\""

-- partiallyAppliedScript :: Integer -> Script
-- partiallyAppliedScript i =
--   fromCompiledCode
--     ( $$(PlutusTx.compile [||mkTestValidatorUntyped||])
--         `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData i)
--     )
-- Script applied with (I 32)
partiallyAppliedScript :: Ledger.Script
partiallyAppliedScript =
  unsafeDecode
    "Script"
    "\"58750100003333222323233222233222225335300c33225335300e0021001100f333500\
    \b22333573466e1c00800404003c0152002333500b22333573466e3c00800404003c011221\
    \0010091326353008009498cd4015d680119a802bae0011200120011200112001122002122\
    \00120014c010218200001\""

-- fullyAppliedScript :: Integer -> CurrencySymbol -> Script
-- fullyAppliedScript i b =
--   getValidator $
--     mkValidatorScript
--       ( $$(PlutusTx.compile [||mkTestValidatorUntyped||])
--           `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData i)
--           `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData b)
--       )
-- Script applied with (I 32) (B "test")
fullyAppliedScript :: Ledger.Script
fullyAppliedScript =
  unsafeDecode
    "Script"
    "\"587f01000033333222323233222233222225335300c33225335300e0021001100f33350\
    \0b22333573466e1c00800404003c0152002333500b22333573466e3c00800404003c01122\
    \010010091326353008009498cd4015d680119a802bae00112001200112001120011220021\
    \2200120014c01021820004c010544746573740001\""
