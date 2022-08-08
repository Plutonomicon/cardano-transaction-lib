-- | `plutip-server` PR:
-- | https://github.com/mlabs-haskell/plutip/pull/79 (run with `cabal run plutip-server`)
module Test.Plutip
  ( main
  ) where

import Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractAffM
  , liftContractM
  , liftedE
  , liftedM
  )
import Contract.PlutusData
  ( PlutusData(Integer)
  , Redeemer(Redeemer)
  , getDatumByHash
  , getDatumsByHashes
  )
import Contract.Prelude (mconcat)
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, validatorHash)
import Contract.Test.Plutip
  ( InitialUTxO
  , runContractInEnv
  , runPlutipContract
  , withPlutipContractEnv
  )
import Contract.Transaction
  ( BalancedSignedTransaction
  , DataHash
  , awaitTxConfirmed
  , balanceAndSignTxM
  , balanceAndSignTxE
  , getTxByHash
  , submit
  , withBalancedAndSignedTxs
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (withResource)
import Control.Monad.Reader (asks)
import Control.Parallel (parallel, sequential)
import Data.BigInt as BigInt
import Data.Log.Level (LogLevel(Trace))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (throw)
import Effect.Ref as Ref
import Examples.AlwaysMints (alwaysMintsPolicy)
import Examples.AlwaysSucceeds as AlwaysSucceeds
import Examples.MintsMultipleTokens
  ( mintingPolicyRdmrInt1
  , mintingPolicyRdmrInt2
  , mintingPolicyRdmrInt3
  )
import Mote (group, test)
import Plutip.Server
  ( startPlutipCluster
  , startPlutipServer
  , stopChildProcessWithPort
  , stopPlutipCluster
  )
import Plutip.Types
  ( PlutipConfig
  , StartClusterResponse(ClusterStartupSuccess)
  , StopClusterResponse(StopClusterSuccess)
  )
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Runner (defaultConfig)
import Test.Utils as Utils
import TestM (TestPlanM)
import Types.UsedTxOuts (TxOutRefCache)

-- Run with `spago test --main Test.Plutip`
main :: Effect Unit
main = launchAff_ do
  Utils.interpretWithConfig
    -- we don't want to exit because we need to clean up after failure by
    -- timeout
    defaultConfig { timeout = Just $ wrap 30_000.0, exit = true }
    suite

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }

suite :: TestPlanM Unit
suite = do
  group "Plutip" do
    test "startPlutipCluster / stopPlutipCluster" do
      withResource (startPlutipServer config)
        (stopChildProcessWithPort config.port) $ const do
        startRes <- startPlutipCluster config unit
        startRes `shouldSatisfy` case _ of
          ClusterStartupSuccess _ -> true
          _ -> false
        liftEffect $ Console.log $ "startPlutipCluster: " <> show startRes
        stopRes <- stopPlutipCluster config
        stopRes `shouldSatisfy` case _ of
          StopClusterSuccess -> true
          _ -> false
        liftEffect $ Console.log $ "stopPlutipCluster: " <> show stopRes

    test "runPlutipContract" do
      let
        distribution :: InitialUTxO /\ InitialUTxO
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ] /\
            [ BigInt.fromInt 2_000_000_000 ]
      runPlutipContract config distribution \(alice /\ bob) -> do
        withKeyWallet alice do
          pure unit -- sign, balance, submit, etc.
        withKeyWallet bob do
          pure unit -- sign, balance, submit, etc.

    test "runPlutipContract: Pkh2Pkh" do
      let
        distribution :: InitialUTxO
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice do
          pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
          let
            constraints :: Constraints.TxConstraints Void Void
            -- In real contracts, library users most likely want to use
            -- `mustPayToPubKeyAddress` (we're not doing that because Plutip
            -- does not provide stake keys).
            constraints = Constraints.mustPayToPubKey pkh
              $ Value.lovelaceValueOf
              $ BigInt.fromInt 2_000_000

            lookups :: Lookups.ScriptLookups Void
            lookups = mempty
          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          bsTx <-
            liftedE $ balanceAndSignTxE ubTx
          submitAndLog bsTx

    test "runPlutipContract: parallel Pkh2Pkh" do
      let
        distribution :: InitialUTxO /\ InitialUTxO
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ] /\
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 2_000_000_000
            ]
      withPlutipContractEnv config distribution \env (alice /\ bob) ->
        sequential ado
          parallel $ runContractInEnv env $ withKeyWallet alice do
            bobPkh <- liftedM "Failed to get PKH" $ withKeyWallet bob
              ownPaymentPubKeyHash
            let
              constraints :: Constraints.TxConstraints Void Void
              -- In real contracts, library users most likely want to use
              -- `mustPayToPubKeyAddress` (we're not doing that because Plutip
              -- does not provide stake keys).
              constraints = Constraints.mustPayToPubKey bobPkh
                $ Value.lovelaceValueOf
                $ BigInt.fromInt 2_000_000

              lookups :: Lookups.ScriptLookups Void
              lookups = mempty
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            bsTx <-
              liftedE $ balanceAndSignTxE ubTx
            submitAndLog bsTx
          parallel $ runContractInEnv env $ withKeyWallet bob do
            alicePkh <- liftedM "Failed to get PKH" $ withKeyWallet alice
              ownPaymentPubKeyHash
            let
              constraints :: Constraints.TxConstraints Void Void
              -- In real contracts, library users most likely want to use
              -- `mustPayToPubKeyAddress` (we're not doing that because Plutip
              -- does not provide stake keys).
              constraints = Constraints.mustPayToPubKey alicePkh
                $ Value.lovelaceValueOf
                $ BigInt.fromInt 2_000_000

              lookups :: Lookups.ScriptLookups Void
              lookups = mempty
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            bsTx <-
              liftedE $ balanceAndSignTxE ubTx
            submitAndLog bsTx
          in unit

    test "runPlutipContract: AlwaysMints" do
      let
        distribution :: InitialUTxO
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice do
          mp <- alwaysMintsPolicy
          cs <- liftContractAffM "Cannot get cs" $ Value.scriptCurrencySymbol mp
          tn <- liftContractM "Cannot make token name"
            $ Value.mkTokenName
                =<< byteArrayFromAscii "TheToken"

          let
            constraints :: Constraints.TxConstraints Void Void
            constraints = Constraints.mustMintValue
              $ Value.singleton cs tn
              $ BigInt.fromInt 100

            lookups :: Lookups.ScriptLookups Void
            lookups = Lookups.mintingPolicy mp

          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          bsTx <-
            liftedM "Failed to balance/sign tx" $ balanceAndSignTxM ubTx
          submitAndLog bsTx

    test "runPlutipContract: Datums" do
      runPlutipContract config unit \_ -> do
        let
          mkDatumHash :: String -> DataHash
          mkDatumHash = wrap <<< hexToByteArrayUnsafe
        -- Nothing is expected, because we are in an empty chain.
        -- This test only checks for ability to connect to ODC
        logInfo' <<< show =<< getDatumByHash
          ( mkDatumHash
              "42be572a6d9a8a2ec0df04f14b0d4fcbe4a7517d74975dfff914514f12316252"
          )
        logInfo' <<< show =<< getDatumsByHashes
          [ mkDatumHash
              "777093fe6dfffdb3bd2033ad71745f5e2319589e36be4bc9c8cca65ac2bfeb8f"
          , mkDatumHash
              "e8cb7d18e81b0be160c114c563c020dcc7bf148a1994b73912db3ea1318d488b"
          ]

    test "runPlutipContract: MintsMultipleTokens" do
      let
        distribution :: InitialUTxO
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice do
          tn1 <- mkTokenName "Token with a long name"
          tn2 <- mkTokenName "Token"
          mp1 /\ cs1 <- mkCurrencySymbol mintingPolicyRdmrInt1
          mp2 /\ cs2 <- mkCurrencySymbol mintingPolicyRdmrInt2
          mp3 /\ cs3 <- mkCurrencySymbol mintingPolicyRdmrInt3

          let
            constraints :: Constraints.TxConstraints Void Void
            constraints = mconcat
              [ Constraints.mustMintValueWithRedeemer
                  (Redeemer $ Integer (BigInt.fromInt 1))
                  (Value.singleton cs1 tn1 one <> Value.singleton cs1 tn2 one)
              , Constraints.mustMintValueWithRedeemer
                  (Redeemer $ Integer (BigInt.fromInt 2))
                  (Value.singleton cs2 tn1 one <> Value.singleton cs2 tn2 one)
              , Constraints.mustMintValueWithRedeemer
                  (Redeemer $ Integer (BigInt.fromInt 3))
                  (Value.singleton cs3 tn1 one <> Value.singleton cs3 tn2 one)
              ]

            lookups :: Lookups.ScriptLookups Void
            lookups =
              Lookups.mintingPolicy mp1
                <> Lookups.mintingPolicy mp2
                <> Lookups.mintingPolicy mp3

          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          bsTx <-
            liftedM "Failed to balance/sign tx" $ balanceAndSignTxM ubTx
          submitAndLog bsTx

    test "runPlutipContract: SignMultiple" do
      let
        distribution :: InitialUTxO
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 100_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice do
          pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
          let
            constraints :: Constraints.TxConstraints Void Void
            -- In real contracts, library users most likely want to use
            -- `mustPayToPubKeyAddres` (we're not doing that because Plutip
            -- does not provide stake keys).
            constraints = Constraints.mustPayToPubKey pkh
              $ Value.lovelaceValueOf
              $ BigInt.fromInt 2_000_000

            lookups :: Lookups.ScriptLookups Void
            lookups = mempty

          ubTx1 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          ubTx2 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints

          withBalancedAndSignedTxs [ ubTx1, ubTx2 ] $ \txs -> do
            locked <- getLockedInputs
            logInfo' $ "Locked inputs inside bracket (should be nonempty): " <>
              show
                locked
            traverse_ submitAndLog txs

          locked <- getLockedInputs
          logInfo' $ "Locked inputs after bracket (should be empty): " <> show
            locked
          unless (locked # Map.isEmpty) do
            liftEffect $ throw "locked inputs map is not empty"

    test "runPlutipContract: AlwaysSucceeds" do
      let
        distribution :: InitialUTxO
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice do
          validator <- AlwaysSucceeds.alwaysSucceedsScript
          vhash <- liftContractAffM "Couldn't hash validator"
            $ validatorHash validator
          logInfo' "Attempt to lock value"
          txId <- AlwaysSucceeds.payToAlwaysSucceeds vhash
          awaitTxConfirmed txId
          logInfo' "Try to spend locked values"
          AlwaysSucceeds.spendFromAlwaysSucceeds vhash validator txId

submitAndLog
  :: forall (r :: Row Type). BalancedSignedTransaction -> Contract r Unit
submitAndLog bsTx = do
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  awaitTxConfirmed txId
  mbTransaction <- getTxByHash txId
  logInfo' $ "Tx: " <> show mbTransaction
  liftEffect $ when (isNothing mbTransaction) do
    void $ throw "Unable to get Tx contents"
    when (mbTransaction /= Just (unwrap bsTx)) do
      throw "Tx contents do not match"

getLockedInputs :: forall (r :: Row Type). Contract r TxOutRefCache
getLockedInputs = do
  cache <- asks (_.usedTxOuts <<< _.runtime <<< unwrap)
  liftEffect $ Ref.read $ unwrap cache

mkTokenName :: forall (r :: Row Type). String -> Contract r TokenName
mkTokenName =
  liftContractM "Cannot make token name"
    <<< (Value.mkTokenName <=< byteArrayFromAscii)

mkCurrencySymbol
  :: forall (r :: Row Type)
   . Contract r MintingPolicy
  -> Contract r (MintingPolicy /\ CurrencySymbol)
mkCurrencySymbol mintingPolicy = do
  mp <- mintingPolicy
  cs <- liftContractAffM "Cannot get cs" $ Value.scriptCurrencySymbol mp
  pure (mp /\ cs)
