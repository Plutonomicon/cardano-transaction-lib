-- | `plutip-server` PR:
-- | https://github.com/mlabs-haskell/plutip/pull/79 (run with `cabal run plutip-server`)
module Test.Plutip
  ( main
  ) where

import Prelude

import Contract.Address
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  , getWalletCollateral
  , ownPaymentPubKeyHash
  )
import Contract.Hashing (nativeScriptHash)
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
  , NativeScript(ScriptAll, ScriptPubkey)
  , TransactionInput(..)
  , awaitTxConfirmed
  , balanceAndSignTx
  , balanceAndSignTxE
  , balanceTx
  , signTransaction
  , getTxByHash
  , submit
  , withBalancedAndSignedTxs
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoM(..), utxosAt)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (withResource)
import Control.Monad.Reader (asks)
import Data.Array (find)
import Data.BigInt as BigInt
import Data.Log.Level (LogLevel(Trace))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isNothing)
import Control.Parallel (parallel, sequential)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse_)
import Data.Tuple (fst)
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
import Mote (group, only, test)
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
import Plutus.Conversion.Address (toPlutusAddress)
import Safe.Coerce (coerce)
import Scripts (nativeScriptHashEnterpriseAddress)
import Plutus.Types.Transaction (TransactionOutput(TransactionOutput))
import Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Plutus.Types.Value (lovelaceValueOf)
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
  only $ group "Plutip" do
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
          getWalletCollateral >>= liftEffect <<< case _ of
            Nothing -> throw "Unable to get collateral"
            Just
              [ TransactionUnspentOutput
                  { output: TransactionOutput { amount } }
              ] -> do
              unless (amount == lovelaceValueOf (BigInt.fromInt 1_000_000_000))
                $ throw "Wrong UTxO selected as collateral"
            Just _ -> do
              -- not a bug, but unexpected
              throw "More than one UTxO in collateral"
        withKeyWallet bob do
          pure unit -- sign, balance, submit, etc.

    only $ test "NativeScript" do
      let
        distribution :: InitialUTxO /\ InitialUTxO /\ InitialUTxO /\ InitialUTxO
        distribution =
          [ BigInt.fromInt 2_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
            /\
              [ BigInt.fromInt 2_000_000_000
              , BigInt.fromInt 2_000_000_000
              ]
            /\
              [ BigInt.fromInt 2_000_000_000
              , BigInt.fromInt 2_000_000_000
              ]
            /\
              [ BigInt.fromInt 2_000_000_000
              , BigInt.fromInt 2_000_000_000
              ]
      runPlutipContract config distribution \(alice /\ bob /\ charlie /\ dan) ->
        do
          alicePaymentPKH <- liftedM "Unable to get Alice's PKH" $
            coerce <$> withKeyWallet alice ownPaymentPubKeyHash
          bobPaymentPKH <- liftedM "Unable to get Bob's PKH" $
            coerce <$> withKeyWallet bob ownPaymentPubKeyHash
          charliePaymentPKH <- liftedM "Unable to get Charlie's PKH" $
            map (unwrap <<< unwrap) <$> withKeyWallet charlie
              ownPaymentPubKeyHash
          danPaymentPKH <- liftedM "Unable to get Dan's PKH" $
            map (unwrap <<< unwrap) <$> withKeyWallet dan ownPaymentPubKeyHash
          let
            nativeScript = ScriptAll
              [ ScriptPubkey alicePaymentPKH
              , ScriptPubkey bobPaymentPKH
              -- , ScriptPubkey charliePaymentPKH
              -- , ScriptPubkey danPaymentPKH
              ]
          nsHash <- liftContractM "Unable to hash NativeScript" $
            nativeScriptHash nativeScript
          -- Alice locks 10 ADA at mutlisig script
          txId <- withKeyWallet alice do
            let
              constraints :: TxConstraints Unit Unit
              constraints = Constraints.mustPayToNativeScript nsHash
                $ Value.lovelaceValueOf
                $ BigInt.fromInt 10_000_000

              lookups :: Lookups.ScriptLookups PlutusData
              lookups = mempty

            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            bsTx <-
              liftedE $ balanceAndSignTxE ubTx
            txId <- submit bsTx
            awaitTxConfirmed txId
            pure txId
          -- Bob attempts to unlock and send Ada to Charlie
          withKeyWallet bob do
            -- First, he should find the transaction input where Ada is locked
            networkId <- asks $ unwrap >>> _.config >>> _.networkId
            let
              nsAddr = nativeScriptHashEnterpriseAddress networkId nsHash

              hasTransactionId :: TransactionInput /\ _ -> Boolean
              hasTransactionId (TransactionInput tx /\ _) =
                tx.transactionId == txId
            nsAddrPlutus <- liftContractM "Unable to convert to Plutus address"
              $ toPlutusAddress nsAddr
            UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt nsAddrPlutus
            txInput <- liftContractM "Unable to get UTxO" $
              fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array _)
            let
              constraints :: TxConstraints Unit Unit
              constraints =
                Constraints.mustPayToPubKey (wrap $ wrap alicePaymentPKH)
                  (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)
                  <> Constraints.mustSpendNativeScriptOutput txInput nsHash
                  <> Constraints.mustBeSignedBy (wrap $ wrap alicePaymentPKH)
                  <> Constraints.mustBeSignedBy (wrap $ wrap bobPaymentPKH)
                  <> Constraints.mustBeSignedBy (wrap $ wrap bobPaymentPKH)

              -- <> Constraints.mustBeSignedBy (wrap $ wrap charliePaymentPKH)
              -- <> Constraints.mustBeSignedBy (wrap $ wrap danPaymentPKH)

              lookups :: Lookups.ScriptLookups PlutusData
              lookups = Lookups.unspentOutputs utxos <>
                Lookups.nativeScript nativeScript

            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            bTx <- liftedE $ map unwrap <$> balanceTx ubTx
            tx <- liftContractM "Unable to sign transaction" =<< signTransaction
              bTx
            tx <- withKeyWallet alice do
              liftContractM "Unable to sign transaction" =<< signTransaction tx
            -- tx <- withKeyWallet charlie do
            --   liftContractM "Unable to sign transaction" =<< signTransaction tx
            -- tx <- withKeyWallet dan do
            --   liftContractM "Unable to sign transaction" =<< signTransaction
            --     tx
            liftEffect $ Console.log $ show tx
            txId <- submit (wrap tx)
            awaitTxConfirmed txId

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
            liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
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
            liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
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
