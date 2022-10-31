-- | `plutip-server` PR:
-- | https://github.com/mlabs-haskell/plutip/pull/79 (run with `cabal run plutip-server`)
module Test.Ctl.Plutip
  ( main
  ) where

import Prelude

import Contract.Address
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  , StakePubKeyHash
  , getWalletAddress
  , getWalletCollateral
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  )
import Contract.BalanceTxConstraints
  ( BalanceTxConstraintsBuilder
  , mustUseAdditionalUtxos
  ) as BalanceTxConstraints
import Contract.Chain (currentTime)
import Contract.Hashing (nativeScriptHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM, wrapContract)
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData(Bytes, Integer, List)
  , Redeemer(Redeemer)
  , getDatumByHash
  , getDatumsByHashes
  , getDatumsByHashesWithErrors
  )
import Contract.Prelude (mconcat)
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (applyArgs, mintingPolicyHash, validatorHash)
import Contract.Test.Plutip
  ( InitialUTxOs
  , runPlutipContract
  , testPlutipContracts'
  , withWallets
  , noWallet
  , PlutipTestM
  , withStakeKey
  )
import Contract.Test.Plutip (group) as Plutip
import Contract.Time (getEraSummaries)
import Contract.Transaction
  ( BalancedSignedTransaction
  , DataHash
  , NativeScript(ScriptPubkey, ScriptNOfK, ScriptAll)
  , ScriptRef(PlutusScriptRef, NativeScriptRef)
  , awaitTxConfirmed
  , balanceTx
  , balanceTxWithConstraints
  , createAdditionalUtxos
  , getTxByHash
  , signTransaction
  , submit
  , withBalancedTx
  , withBalancedTxs
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletBalance, utxosAt)
import Contract.Value (Coin(Coin), coinToValue)
import Contract.Value as Value
import Contract.Wallet
  ( getWalletUtxos
  , isFlintAvailable
  , isGeroAvailable
  , isNamiAvailable
  , withKeyWallet
  )
import Control.Monad.Error.Class (try)
import Control.Monad.Reader (asks)
import Control.Parallel (parallel, sequential)
import Ctl.Examples.AlwaysMints (alwaysMintsPolicy)
import Ctl.Examples.AlwaysSucceeds as AlwaysSucceeds
import Ctl.Examples.AwaitTxConfirmedWithTimeout as AwaitTxConfirmedWithTimeout
import Ctl.Examples.BalanceTxConstraints as BalanceTxConstraintsExample
import Ctl.Examples.ContractTestUtils as ContractTestUtils
import Ctl.Examples.Helpers
  ( mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  )
import Ctl.Examples.IncludeDatum as IncludeDatum
import Ctl.Examples.Lose7Ada as AlwaysFails
import Ctl.Examples.MintsMultipleTokens
  ( mintingPolicyRdmrInt1
  , mintingPolicyRdmrInt2
  , mintingPolicyRdmrInt3
  )
import Ctl.Examples.NativeScriptMints (contract) as NativeScriptMints
import Ctl.Examples.OneShotMinting (contract) as OneShotMinting
import Ctl.Examples.PlutusV2.AlwaysSucceeds as AlwaysSucceedsV2
import Ctl.Examples.PlutusV2.InlineDatum as InlineDatum
import Ctl.Examples.PlutusV2.OneShotMinting (contract) as OneShotMintingV2
import Ctl.Examples.PlutusV2.ReferenceInputs (alwaysMintsPolicyV2)
import Ctl.Examples.PlutusV2.ReferenceInputs (contract) as ReferenceInputs
import Ctl.Examples.PlutusV2.ReferenceScripts (contract) as ReferenceScripts
import Ctl.Examples.SendsToken (contract) as SendsToken
import Ctl.Examples.TxChaining (contract) as TxChaining
import Ctl.Internal.Plutip.Server
  ( checkPlutipServer
  , startPlutipCluster
  , startPlutipServer
  , stopChildProcessWithPort
  , stopPlutipCluster
  )
import Ctl.Internal.Plutip.Types
  ( InitialUTxOsWithStakeKey
  , StopClusterResponse(StopClusterSuccess)
  )
import Ctl.Internal.Plutip.UtxoDistribution (class UtxoDistribution)
import Ctl.Internal.Plutus.Conversion.Address (toPlutusAddress)
import Ctl.Internal.Plutus.Types.Transaction
  ( TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  , _input
  , lookupTxHash
  )
import Ctl.Internal.Plutus.Types.Value (lovelaceValueOf)
import Ctl.Internal.Scripts (nativeScriptHashEnterpriseAddress)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Test.TestPlanM as Utils
import Ctl.Internal.Types.Interval (getSlotLength)
import Ctl.Internal.Types.UsedTxOuts (TxOutRefCache)
import Ctl.Internal.Wallet.Cip30Mock
  ( WalletMock(MockNami, MockGero, MockFlint)
  , withCip30Mock
  )
import Ctl.Internal.Wallet.Key (KeyWallet)
import Data.Array (replicate, (!!))
import Data.BigInt as BigInt
import Data.Either (isLeft)
import Data.Foldable (fold, foldM, length)
import Data.Lens (view)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Mote (group, skip, test)
import Mote.Monad (mapTest)
import Safe.Coerce (coerce)
import Test.Ctl.AffInterface as AffInterface
import Test.Ctl.Fixtures
  ( cip25MetadataFixture1
  , fullyAppliedScriptFixture
  , nativeScriptFixture1
  , nativeScriptFixture2
  , nativeScriptFixture3
  , nativeScriptFixture4
  , nativeScriptFixture5
  , nativeScriptFixture6
  , nativeScriptFixture7
  , partiallyAppliedScriptFixture
  , unappliedScriptFixture
  )
import Test.Ctl.Plutip.Common (config, privateStakeKey)
import Test.Ctl.Plutip.Logging as Logging
import Test.Ctl.Plutip.NetworkId as NetworkId
import Test.Ctl.Plutip.UtxoDistribution (checkUtxoDistribution)
import Test.Ctl.Plutip.UtxoDistribution as UtxoDistribution
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, shouldSatisfy)
import Test.Spec.Runner (defaultConfig)

-- Run with `spago test --main Test.Ctl.Plutip`
main :: Effect Unit
main = launchAff_ do
  Utils.interpretWithConfig
    defaultConfig { timeout = Just $ wrap 70_000.0, exit = true }
    do
      suite
      UtxoDistribution.suite
      NetworkId.suite

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Plutip" do
    Logging.suite

    test "startPlutipCluster / stopPlutipCluster" do
      bracket (startPlutipServer config)
        (stopChildProcessWithPort config.port) $ const do
        checkPlutipServer config
        _startRes <- startPlutipCluster config [ [] ]
        stopRes <- stopPlutipCluster config
        stopRes `shouldSatisfy` case _ of
          StopClusterSuccess -> true
          _ -> false
    
    -- TODO Don't run separately, there's a bug with array distributions
    --      Even this isn't enough
    testPlutipContracts' config do
      testUtxoDistributionArray

    testPlutipContracts' config do
      testAffInterface
      testAwaitTxConfirmedWithTimeout
      testDatums
      testCurrentTime
      testApplyArgs
      testContract
      testPkh2Pkh
      testNativeScript
      testAlwaysMints
      testNativeScriptMints
      testMintsMultipleTokens
      testSignMultiple
      testAlwaysSucceeds
      testSendsToken
      testInlineDatum
      testIncludeDatum
      testAlwaysSucceedsV2
      testAlwaysFails
      testReferenceScripts
      testReferenceInputs
      testOneShotMinting
      testOneShotMintingPlutusV2
      testContractTestUtils
      testBalanceTxConstraints
      testAdditionalUtxosTxChaining
      testCip30Mock

testAffInterface :: PlutipTestM Unit
testAffInterface =
  noWallet $ mapTest wrapContract AffInterface.suite

testAwaitTxConfirmedWithTimeout :: PlutipTestM Unit
testAwaitTxConfirmedWithTimeout =
  noWallet $ test "runPlutipContract: awaitTxConfirmedWithTimeout fails after timeout" AwaitTxConfirmedWithTimeout.contract

testDatums :: PlutipTestM Unit
testDatums =
  noWallet do
    test "runPlutipContract: Datums" do
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
      logInfo' <<< show =<< getDatumsByHashesWithErrors
        [ mkDatumHash
            "777093fe6dfffdb3bd2033ad71745f5e2319589e36be4bc9c8cca65ac2bfeb8f"
        , mkDatumHash
            "e8cb7d18e81b0be160c114c563c020dcc7bf148a1994b73912db3ea1318d488b"
        ]

testCurrentTime :: PlutipTestM Unit
testCurrentTime =
  noWallet do
    test "runPlutipContract: currentTime" do
      void $ currentTime
      void $ getEraSummaries >>= unwrap >>> traverse
        (getSlotLength >>> show >>> logInfo')

testApplyArgs :: PlutipTestM Unit
testApplyArgs =
  noWallet do
    group "applyArgs" do
      test "returns the same script when called without args" do
        result <- liftedE $ applyArgs (unwrap unappliedScriptFixture) mempty
        result `shouldEqual` (unwrap unappliedScriptFixture)

      test "returns the correct partially applied Plutus script" do
        let args = [ Integer (BigInt.fromInt 32) ]
        result <- liftedE $ applyArgs (unwrap unappliedScriptFixture) args
        result `shouldEqual` (unwrap partiallyAppliedScriptFixture)

      test "returns the correct fully applied Plutus script" do
        bytes <-
          liftContractM "Could not create ByteArray"
            (byteArrayFromAscii "test")
        let args = [ Integer (BigInt.fromInt 32), Bytes bytes ]
        result <- liftedE $ applyArgs (unwrap unappliedScriptFixture) args
        result `shouldEqual` (unwrap fullyAppliedScriptFixture)

testContract :: PlutipTestM Unit
testContract = do
  let
    distribution :: InitialUTxOs /\ InitialUTxOs
    distribution =
      [ BigInt.fromInt 1_000_000_000
      , BigInt.fromInt 2_000_000_000
      ] /\
        [ BigInt.fromInt 2_000_000_000 ]
  withWallets distribution $ test "runPlutipContract" \(alice /\ bob) -> do
    withKeyWallet alice do
      getWalletCollateral >>= liftEffect <<< case _ of
        Nothing -> throw "Unable to get collateral"
        Just
          [ TransactionUnspentOutput
              { output: TransactionOutputWithRefScript { output } }
          ] -> do
          let amount = (unwrap output).amount
          unless (amount == lovelaceValueOf (BigInt.fromInt 1_000_000_000))
            $ throw "Wrong UTxO selected as collateral"
        Just _ -> do
          -- not a bug, but unexpected
          throw "More than one UTxO in collateral"
    withKeyWallet bob do
      pure unit -- sign, balance, submit, etc.

withCheckedWallets :: forall distr wallets. UtxoDistribution distr wallets => distr -> TestPlanM (wallets -> Contract () Unit) Unit -> PlutipTestM Unit
withCheckedWallets distr tests = withWallets distr do
  flip mapTest tests \test wallets -> do
    checkUtxoDistribution distr wallets
    test wallets

testPkh2Pkh :: PlutipTestM Unit
testPkh2Pkh = do
  let
    aliceUtxos :: InitialUTxOs
    aliceUtxos =
      [ BigInt.fromInt 1_000_000_000
      , BigInt.fromInt 2_000_000_000
      ]

    bobUtxos :: InitialUTxOs
    bobUtxos =
      [ BigInt.fromInt 1_000_000_000
      , BigInt.fromInt 2_000_000_000
      ]

    aliceUtxosWithStake :: InitialUTxOsWithStakeKey
    aliceUtxosWithStake = withStakeKey privateStakeKey aliceUtxos

    bobUtxosWithStake :: InitialUTxOsWithStakeKey
    bobUtxosWithStake = withStakeKey privateStakeKey bobUtxos

  withCheckedWallets aliceUtxos do
    test "runPlutipContract: Pkh2Pkh" \alice -> do
      pkh <- liftedM "Failed to get PKH" $ withKeyWallet alice
        ownPaymentPubKeyHash
      stakePkh <- withKeyWallet alice ownStakePubKeyHash
      withKeyWallet alice $ pkh2PkhContract pkh stakePkh

  withCheckedWallets aliceUtxosWithStake do
    test "runPlutipContract: Pkh2Pkh with stake key" \alice -> do
      pkh <- liftedM "Failed to get PKH" $ withKeyWallet alice
        ownPaymentPubKeyHash
      stakePkh <- withKeyWallet alice ownStakePubKeyHash
      stakePkh `shouldSatisfy` isJust
      withKeyWallet alice $ pkh2PkhContract pkh stakePkh
  
  withCheckedWallets (aliceUtxos /\ bobUtxos) do
    test "runPlutipContract: parallel Pkh2Pkh" \(alice /\ bob) -> do
      sequential ado
        parallel $ withKeyWallet alice do
          pkh <- liftedM "Failed to get PKH" $ withKeyWallet bob
            ownPaymentPubKeyHash
          stakePkh <- withKeyWallet bob ownStakePubKeyHash
          pkh2PkhContract pkh stakePkh
        parallel $ withKeyWallet bob do
          pkh <- liftedM "Failed to get PKH" $ withKeyWallet alice
            ownPaymentPubKeyHash
          stakePkh <- withKeyWallet alice ownStakePubKeyHash
          pkh2PkhContract pkh stakePkh
        in unit

  withCheckedWallets (aliceUtxosWithStake /\ bobUtxosWithStake) do
    test "runPlutipContract: parallel Pkh2Pkh with stake keys" \(alice /\ bob) -> do
      sequential ado
        parallel $ withKeyWallet alice do
          pkh <- liftedM "Failed to get PKH" $ withKeyWallet bob
            ownPaymentPubKeyHash
          stakePkh <- withKeyWallet bob ownStakePubKeyHash
          pkh2PkhContract pkh stakePkh
        parallel $ withKeyWallet bob do
          pkh <- liftedM "Failed to get PKH" $ withKeyWallet alice
            ownPaymentPubKeyHash
          stakePkh <- withKeyWallet alice ownStakePubKeyHash
          pkh2PkhContract pkh stakePkh
        in unit

-- TODO Move to UtxoDistribution suite
testUtxoDistributionArray :: PlutipTestM Unit
testUtxoDistributionArray = do
  let
    distribution :: Array InitialUTxOs
    distribution = replicate 2 [ BigInt.fromInt 1_000_000_000 ]

  withWallets distribution $ test "runPlutipContract: Array of InitialUTxOs and KeyWallet" do
    arrayTest

  let
    distribution' :: Array InitialUTxOsWithStakeKey
    distribution' = withStakeKey privateStakeKey <$> distribution

  withWallets distribution' $ test "runPlutipContract: Array of InitialUTxOsWithStakeKey and KeyWallet" do
    arrayTest
  where
  arrayTest
    :: Array KeyWallet
    -> Contract () Unit
  arrayTest wallets = do
      traverse_
        ( \wallet -> do
            withKeyWallet wallet do
              getWalletCollateral >>= liftEffect <<< case _ of
                Nothing -> throw "Unable to get collateral"
                Just
                  [ TransactionUnspentOutput
                      { output: TransactionOutputWithRefScript { output } }
                  ] -> do
                  let amount = (unwrap output).amount
                  unless
                    ( amount == lovelaceValueOf
                        (BigInt.fromInt 1_000_000_000)
                    )
                    $ throw "Wrong UTxO selected as collateral"
                Just _ -> do
                  -- not a bug, but unexpected
                  throw "More than one UTxO in collateral"
        )
        wallets

testNativeScript :: PlutipTestM Unit
testNativeScript = do
  let
    distribution
      :: InitialUTxOs /\ InitialUTxOs /\ InitialUTxOs /\ InitialUTxOs
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
  
  withWallets distribution $ test "NativeScript: require all signers"
    \(alice /\ bob /\ charlie /\ dan) -> do
      alicePaymentPKH <- liftedM "Unable to get Alice's PKH" $
        coerce <$> withKeyWallet alice ownPaymentPubKeyHash
      bobPaymentPKH <- liftedM "Unable to get Bob's PKH" $
        coerce <$> withKeyWallet bob ownPaymentPubKeyHash
      charliePaymentPKH <- liftedM "Unable to get Charlie's PKH" $
        coerce <$> withKeyWallet charlie
          ownPaymentPubKeyHash
      danPaymentPKH <- liftedM "Unable to get Dan's PKH" $
        coerce <$> withKeyWallet dan ownPaymentPubKeyHash
      let
        nativeScript = ScriptAll
          [ ScriptPubkey alicePaymentPKH
          , ScriptPubkey bobPaymentPKH
          , ScriptPubkey charliePaymentPKH
          , ScriptPubkey danPaymentPKH
          ]
        nsHash = nativeScriptHash nativeScript

      -- Alice locks 10 ADA at multisig script
      txId <- withKeyWallet alice do
        let
          constraints :: TxConstraints Unit Unit
          constraints = Constraints.mustPayToNativeScript nsHash
            $ Value.lovelaceValueOf
            $ BigInt.fromInt 10_000_000

          lookups :: Lookups.ScriptLookups PlutusData
          lookups = mempty

        ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
        bsTx <- signTransaction =<< liftedE (balanceTx ubTx)
        txId <- submit bsTx
        awaitTxConfirmed txId
        pure txId
      -- Bob attempts to unlock and send Ada to Charlie
      withKeyWallet bob do
        -- First, he should find the transaction input where Ada is locked
        networkId <- asks $ unwrap >>> _.config >>> _.networkId
        let
          nsAddr = nativeScriptHashEnterpriseAddress networkId nsHash
        nsAddrPlutus <- liftContractM "Unable to convert to Plutus address"
          $ toPlutusAddress nsAddr
        utxos <- fromMaybe Map.empty <$> utxosAt nsAddrPlutus
        txInput <- liftContractM "Unable to get UTxO" $
          view _input <$> lookupTxHash txId utxos !! 0
        let
          constraints :: TxConstraints Unit Unit
          constraints =
            Constraints.mustPayToPubKey (coerce alicePaymentPKH)
              (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)
              <> Constraints.mustSpendNativeScriptOutput txInput
                nativeScript

          -- Note that specifying required signers is optional:
          --
          -- <> Constraints.mustBeSignedBy (coerce alicePaymentPKH)
          -- <> Constraints.mustBeSignedBy (coerce bobPaymentPKH)
          -- <> Constraints.mustBeSignedBy (coerce charliePaymentPKH)
          -- <> Constraints.mustBeSignedBy (coerce danPaymentPKH)
          --
          -- The maximum needed number of signers is calculated from
          -- the script itself, so we know how much space to allocate
          -- for signatures on fee calculation stage.

          lookups :: Lookups.ScriptLookups PlutusData
          lookups = Lookups.unspentOutputs utxos

        ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
        tx <- signTransaction =<< liftedE (balanceTx ubTx)
        let
          signWithWallet txToSign wallet =
            withKeyWallet wallet (signTransaction txToSign)
        txSigned <- foldM signWithWallet tx [ alice, bob, charlie, dan ]
        submit txSigned >>= awaitTxConfirmed

  withWallets distribution $ test "NativeScript: NOfK (2)" 
    \(alice /\ bob /\ charlie /\ dan) -> do
      alicePaymentPKH <- liftedM "Unable to get Alice's PKH" $
        coerce <$> withKeyWallet alice ownPaymentPubKeyHash
      bobPaymentPKH <- liftedM "Unable to get Bob's PKH" $
        coerce <$> withKeyWallet bob ownPaymentPubKeyHash
      charliePaymentPKH <- liftedM "Unable to get Charlie's PKH" $
        coerce <$> withKeyWallet charlie
          ownPaymentPubKeyHash
      danPaymentPKH <- liftedM "Unable to get Dan's PKH" $
        coerce <$> withKeyWallet dan ownPaymentPubKeyHash
      let
        nativeScript = ScriptNOfK 2
          [ ScriptPubkey alicePaymentPKH
          , ScriptPubkey bobPaymentPKH
          , ScriptPubkey charliePaymentPKH
          , ScriptPubkey danPaymentPKH
          ]
        nsHash = nativeScriptHash nativeScript

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
        bsTx <- signTransaction =<< liftedE (balanceTx ubTx)
        txId <- submit bsTx
        awaitTxConfirmed txId
        pure txId

      -- Bob attempts to unlock and send Ada to Charlie
      withKeyWallet bob do
        -- First, he should find the transaction input where Ada is locked
        networkId <- asks $ unwrap >>> _.config >>> _.networkId
        let
          nsAddr = nativeScriptHashEnterpriseAddress networkId nsHash
        nsAddrPlutus <- liftContractM "Unable to convert to Plutus address"
          $ toPlutusAddress nsAddr
        utxos <- fromMaybe Map.empty <$> utxosAt nsAddrPlutus
        txInput <- liftContractM "Unable to get UTxO" $
          view _input <$> lookupTxHash txId utxos !! 0
        let
          constraints :: TxConstraints Unit Unit
          constraints =
            Constraints.mustPayToPubKey (coerce alicePaymentPKH)
              (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)
              <> Constraints.mustSpendNativeScriptOutput txInput
                nativeScript

          lookups :: Lookups.ScriptLookups PlutusData
          lookups = Lookups.unspentOutputs utxos

        ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
        -- Bob signs the tx
        tx <- signTransaction =<< liftedE (balanceTx ubTx)
        let
          signWithWallet txToSign wallet =
            withKeyWallet wallet (signTransaction txToSign)
        -- Dan signs the tx
        txSigned <- foldM signWithWallet tx [ dan ]
        submit txSigned >>= awaitTxConfirmed

testAlwaysMints :: PlutipTestM Unit
testAlwaysMints = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]
  withWallets distribution $ test "runPlutipContract: AlwaysMints" \alice -> do
    withKeyWallet alice do
      mp <- alwaysMintsPolicy
      cs <- liftContractM "Cannot get cs" $ Value.scriptCurrencySymbol mp
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
      bsTx <- signTransaction =<< liftedE (balanceTx ubTx)
      submitAndLog bsTx

testNativeScriptMints :: PlutipTestM Unit
testNativeScriptMints = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]

  withWallets distribution $ test "runPlutipContract: NativeScriptMints"
    \alice -> withKeyWallet alice NativeScriptMints.contract

testMintsMultipleTokens :: PlutipTestM Unit
testMintsMultipleTokens = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]
  withWallets distribution $ test "runPlutipContract: MintsMultipleTokens" \alice -> do
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
      bsTx <- signTransaction =<< liftedE (balanceTx ubTx)
      submitAndLog bsTx

testSignMultiple :: PlutipTestM Unit
testSignMultiple = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 100_000_000
      ]

  withCheckedWallets distribution $ test "runPlutipContract: SignMultiple" \alice -> do
    withKeyWallet alice signMultipleContract

  withCheckedWallets (withStakeKey privateStakeKey distribution) do
    test "runPlutipContract: SignMultiple with stake key" \alice -> do
      withKeyWallet alice signMultipleContract

testAlwaysSucceeds :: PlutipTestM Unit
testAlwaysSucceeds = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]
  withWallets distribution $ test "runPlutipContract: AlwaysSucceeds" \alice -> do
    withKeyWallet alice do
      validator <- AlwaysSucceeds.alwaysSucceedsScript
      let vhash = validatorHash validator
      logInfo' "Attempt to lock value"
      txId <- AlwaysSucceeds.payToAlwaysSucceeds vhash
      awaitTxConfirmed txId
      logInfo' "Try to spend locked values"
      AlwaysSucceeds.spendFromAlwaysSucceeds vhash validator txId

testSendsToken :: PlutipTestM Unit
testSendsToken = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]

  withWallets distribution $ test "runPlutipContract: SendsToken" \alice -> do
    withKeyWallet alice SendsToken.contract

testInlineDatum :: PlutipTestM Unit
testInlineDatum = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]

  withWallets distribution $ test "runPlutipContract: InlineDatum" \alice -> do
    withKeyWallet alice do
      validator <- InlineDatum.checkDatumIsInlineScript
      let vhash = validatorHash validator
      logInfo' "Attempt to lock value with inline datum"
      txId <- InlineDatum.payToCheckDatumIsInline vhash
      awaitTxConfirmed txId
      logInfo' "Try to spend locked values"
      InlineDatum.spendFromCheckDatumIsInline vhash validator txId

  withWallets distribution $ test "runPlutipContract: InlineDatum Read" \alice -> do
    withKeyWallet alice do
      validator <- InlineDatum.checkDatumIsInlineScript
      let vhash = validatorHash validator
      logInfo' "Attempt to lock value with inline datum"
      txId <- InlineDatum.payToCheckDatumIsInline vhash
      awaitTxConfirmed txId
      logInfo' "Try to read inline datum"
      InlineDatum.readFromCheckDatumIsInline vhash txId

  withWallets distribution $ test "runPlutipContract: InlineDatum Failure" \alice -> do
    withKeyWallet alice do
      validator <- InlineDatum.checkDatumIsInlineScript
      let vhash = validatorHash validator
      logInfo' "Attempt to lock value without inline datum"
      txId <- InlineDatum.payToCheckDatumIsInlineWrong vhash
      awaitTxConfirmed txId
      logInfo' "Try to spend locked values"
      eResult <- try $ InlineDatum.spendFromCheckDatumIsInline vhash
        validator
        txId
      eResult `shouldSatisfy` isLeft

  withWallets distribution $ test "runPlutipContract: InlineDatum Cannot Spend PlutusV1" \alice -> do
    withKeyWallet alice do
      validator <- AlwaysSucceeds.alwaysSucceedsScript
      let vhash = validatorHash validator
      logInfo' "Attempt to lock value at plutusv1 script with inline datum"
      txId <- InlineDatum.payToCheckDatumIsInline vhash
      awaitTxConfirmed txId
      logInfo' "Try to spend locked values"
      eResult <- try $ InlineDatum.spendFromCheckDatumIsInline vhash
        validator
        txId
      eResult `shouldSatisfy` isLeft

testIncludeDatum :: PlutipTestM Unit
testIncludeDatum = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]
  withWallets distribution $ test "runPlutipContract: IncludeDatum" \alice -> do
    withKeyWallet alice do
      validator <- IncludeDatum.only42Script
      let vhash = validatorHash validator
      logInfo' "Attempt to lock value"
      txId <- IncludeDatum.payToIncludeDatum vhash
      awaitTxConfirmed txId
      logInfo' "Try to spend locked values"
      IncludeDatum.spendFromIncludeDatum vhash validator txId

testAlwaysSucceedsV2 :: PlutipTestM Unit
testAlwaysSucceedsV2 = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]

  withWallets distribution $ test "runPlutipContract: AlwaysSucceeds PlutusV2" \alice -> do
    withKeyWallet alice do
      validator <- AlwaysSucceedsV2.alwaysSucceedsScriptV2
      let vhash = validatorHash validator
      logInfo' "Attempt to lock value"
      txId <- AlwaysSucceeds.payToAlwaysSucceeds vhash
      awaitTxConfirmed txId
      logInfo' "Try to spend locked values"
      AlwaysSucceeds.spendFromAlwaysSucceeds vhash validator txId

testAlwaysFails :: PlutipTestM Unit
testAlwaysFails = do
  let
    distribution :: InitialUTxOs /\ InitialUTxOs
    distribution =
      [ BigInt.fromInt 10_000_000
      , BigInt.fromInt 2_000_000_000
      ] /\ [ BigInt.fromInt 2_000_000_000 ]

  withWallets distribution $ test "runPlutipContract: AlwaysFails Ada Collateral Return"  \(alice /\ seed) -> do
    validator <- AlwaysFails.alwaysFailsScript
    let vhash = validatorHash validator
    txId <- withKeyWallet seed do
      logInfo' "Attempt to lock value"
      txId <- AlwaysFails.payToAlwaysFails vhash
      awaitTxConfirmed txId
      pure txId

    withKeyWallet alice do
      awaitTxConfirmed txId
      logInfo' "Try to spend locked values"
      balanceBefore <- fold <$> getWalletBalance
      AlwaysFails.spendFromAlwaysFails vhash validator txId
      balance <- fold <$> getWalletBalance
      let
        collateralLoss = Value.lovelaceValueOf $ BigInt.fromInt $ -5_000_000
      balance `shouldEqual` (balanceBefore <> collateralLoss)

  let
    distributionNative :: InitialUTxOs /\ InitialUTxOs
    distributionNative =
      [] /\ [ BigInt.fromInt 2_100_000_000 ]

  withWallets distributionNative $ test "runPlutipContract: AlwaysFails Native Asset Collateral Return" \(alice /\ seed) -> do
    alicePkh /\ aliceStakePkh <- withKeyWallet alice do
      pkh <- liftedM "Failed to get PKH" $ ownPaymentPubKeyHash
      stakePkh <- ownStakePubKeyHash
      pure $ pkh /\ stakePkh

    mp <- alwaysMintsPolicy
    cs <- liftContractM "Cannot get cs" $ Value.scriptCurrencySymbol mp
    tn <- liftContractM "Cannot make token name"
      $ byteArrayFromAscii "TheToken" >>= Value.mkTokenName
    let asset = Value.singleton cs tn $ BigInt.fromInt 50

    validator <- AlwaysFails.alwaysFailsScript
    let vhash = validatorHash validator

    txId <- withKeyWallet seed do
      logInfo' "Minting asset to Alice"
      let
        constraints :: Constraints.TxConstraints Void Void
        constraints = Constraints.mustMintValue (asset <> asset)
          <> mustPayToPubKeyStakeAddress alicePkh aliceStakePkh
            (asset <> (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000))
          <> mustPayToPubKeyStakeAddress alicePkh aliceStakePkh
            ( asset <>
                (Value.lovelaceValueOf $ BigInt.fromInt 2_000_000_000)
            )

        lookups :: Lookups.ScriptLookups Void
        lookups = Lookups.mintingPolicy mp

      ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
      bsTx <- signTransaction =<< liftedE (balanceTx ubTx)
      submit bsTx >>= awaitTxConfirmed

      logInfo' "Attempt to lock value"
      txId <- AlwaysFails.payToAlwaysFails vhash
      awaitTxConfirmed txId
      pure txId

    withKeyWallet alice do
      awaitTxConfirmed txId
      logInfo' "Try to spend locked values"
      AlwaysFails.spendFromAlwaysFails vhash validator txId

testReferenceScripts :: PlutipTestM Unit
testReferenceScripts = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]

  withWallets distribution $ test "runPlutipContract: ReferenceScripts" \alice -> do
    withKeyWallet alice ReferenceScripts.contract

testReferenceInputs :: PlutipTestM Unit
testReferenceInputs = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]

  withWallets distribution $ test "runPlutipContract: ReferenceInputs" \alice -> do
    withKeyWallet alice ReferenceInputs.contract

testOneShotMinting :: PlutipTestM Unit
testOneShotMinting = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]
  withWallets distribution $ test "runPlutipContract: OneShotMinting" \alice -> do
    withKeyWallet alice OneShotMinting.contract

testOneShotMintingPlutusV2 :: PlutipTestM Unit
testOneShotMintingPlutusV2 = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]
  withWallets distribution $ test "runPlutipContract: OneShotMinting PlutusV2" \alice -> do
    withKeyWallet alice OneShotMintingV2.contract

testContractTestUtils :: PlutipTestM Unit
testContractTestUtils = do
  let
    initialUtxos :: InitialUTxOs
    initialUtxos =
      [ BigInt.fromInt 2_000_000_000, BigInt.fromInt 2_000_000_000 ]

    distribution :: InitialUTxOs /\ InitialUTxOs
    distribution = initialUtxos /\ initialUtxos

  withWallets distribution $ test "runPlutipContract: Examples.ContractTestUtils" \(alice /\ bob) -> do
    receiverPkh <- liftedM "Unable to get Bob's PKH" $
      withKeyWallet bob ownPaymentPubKeyHash
    receiverSkh <- withKeyWallet bob ownStakePubKeyHash

    mintingPolicy /\ cs <- mkCurrencySymbol alwaysMintsPolicyV2

    tn <- mkTokenName "TheToken"

    withKeyWallet alice $ ContractTestUtils.contract $
      ContractTestUtils.ContractParams
        { receiverPkh
        , receiverSkh
        , adaToSend: BigInt.fromInt 5_000_000
        , mintingPolicy
        , tokensToMint: cs /\ tn /\ one /\ unit
        , datumToAttach: wrap $ Integer $ BigInt.fromInt 42
        , txMetadata: cip25MetadataFixture1
        }

testBalanceTxConstraints :: PlutipTestM Unit
testBalanceTxConstraints = do
  let
    initialUtxos :: InitialUTxOs
    initialUtxos =
      [ BigInt.fromInt 2_000_000_000, BigInt.fromInt 2_000_000_000 ]

    distribution :: InitialUTxOs /\ InitialUTxOs
    distribution = initialUtxos /\ initialUtxos

  withWallets distribution $ test "runPlutipContract: Examples.BalanceTxConstraints" \(alice /\ bob) -> do
    withKeyWallet alice $ BalanceTxConstraintsExample.contract $
      BalanceTxConstraintsExample.ContractParams
        { aliceKeyWallet: alice, bobKeyWallet: bob }

testAdditionalUtxosTxChaining :: PlutipTestM Unit
testAdditionalUtxosTxChaining = do
  let
    distribution :: InitialUTxOs
    distribution = [ BigInt.fromInt 2_500_000 ]

    largeDistribution :: InitialUTxOs
    largeDistribution = [ BigInt.fromInt 150_000_000 ]

  Plutip.group "Evaluation with additional UTxOs and tx chaining" do
    withWallets distribution $ test "runPlutipContract: Examples.TxChaining" \alice -> do
      withKeyWallet alice TxChaining.contract

    withWallets largeDistribution $ test "Evaluation with additional UTxOs with native scripts" \alice -> do
      withKeyWallet alice do
        pkh <- liftedM "Failed to get PKH" $ ownPaymentPubKeyHash

        let
          constraints0 :: TxConstraints Unit Unit
          constraints0 =
            Constraints.mustPayToPubKeyWithScriptRef
              pkh
              (NativeScriptRef nativeScriptFixture1)
              (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)
              <>
                Constraints.mustPayToPubKeyWithScriptRef
                  pkh
                  (NativeScriptRef nativeScriptFixture2)
                  (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)
              <>
                Constraints.mustPayToPubKeyWithScriptRef
                  pkh
                  (NativeScriptRef nativeScriptFixture3)
                  (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)
              <>
                Constraints.mustPayToPubKeyWithScriptRef
                  pkh
                  (NativeScriptRef nativeScriptFixture4)
                  (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)
              <>
                Constraints.mustPayToPubKeyWithScriptRef
                  pkh
                  (NativeScriptRef nativeScriptFixture5)
                  (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)
              <>
                Constraints.mustPayToPubKeyWithScriptRef
                  pkh
                  (NativeScriptRef nativeScriptFixture6)
                  (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)
              <>
                Constraints.mustPayToPubKeyWithScriptRef
                  pkh
                  (NativeScriptRef nativeScriptFixture7)
                  (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)

          lookups0 :: Lookups.ScriptLookups PlutusData
          lookups0 = mempty

        unbalancedTx0 <-
          liftedE $ Lookups.mkUnbalancedTx lookups0 constraints0

        withBalancedTx unbalancedTx0 \balancedTx0 -> do
          balancedSignedTx0 <- signTransaction balancedTx0

          additionalUtxos <- createAdditionalUtxos balancedSignedTx0

          logInfo' $ "Additional utxos: " <> show additionalUtxos
          length additionalUtxos `shouldNotEqual` 0

          let
            constraints1 :: TxConstraints Unit Unit
            constraints1 =
              Constraints.mustPayToPubKey pkh
                (Value.lovelaceValueOf $ BigInt.fromInt 70_000_000)

            lookups1 :: Lookups.ScriptLookups PlutusData
            lookups1 = Lookups.unspentOutputs additionalUtxos

            balanceTxConstraints
              :: BalanceTxConstraints.BalanceTxConstraintsBuilder
            balanceTxConstraints =
              BalanceTxConstraints.mustUseAdditionalUtxos additionalUtxos

          unbalancedTx1 <-
            liftedE $ Lookups.mkUnbalancedTx lookups1 constraints1
          balancedTx1 <-
            liftedE $ balanceTxWithConstraints unbalancedTx1
              balanceTxConstraints
          balancedSignedTx1 <- signTransaction balancedTx1

          txId0 <- submit balancedSignedTx0
          txId1 <- submit balancedSignedTx1

          awaitTxConfirmed txId0
          awaitTxConfirmed txId1

    withWallets largeDistribution $ test "Evaluation with additional UTxOs" \alice ->
      -- We create two transactions. First, we create outputs with Ada, non-Ada
      -- assets, script reference with Plutus script v1 and v2, inline datum,
      -- and datum with its witness. Then, we take those outputs as additional
      -- utxos for the next transaction. After both transactions are balanced
      -- and signed, we submit them.
      withKeyWallet alice do
        pkh <- liftedM "Failed to get PKH" $ ownPaymentPubKeyHash

        wUtxos0 <- liftedM "Failed to get wallet UTXOs" getWalletUtxos
        logInfo' $ "wUtxos0 " <> show wUtxos0

        mp <- alwaysMintsPolicyV2
        cs <- liftContractM "Cannot get cs" $ Value.scriptCurrencySymbol mp
        tn <- liftContractM "Cannot make token name"
          $ byteArrayFromAscii "TheToken" >>= Value.mkTokenName

        validatorV1 <- AlwaysSucceeds.alwaysSucceedsScript
        validatorV2 <- AlwaysSucceedsV2.alwaysSucceedsScriptV2

        let
          value :: Value.Value
          value =
            (Value.lovelaceValueOf $ BigInt.fromInt 60_000_000)

          value' :: Value.Value
          value' =
            value
              <> (Value.singleton cs tn $ BigInt.fromInt 50)

          scriptRefV1 :: ScriptRef
          scriptRefV1 = PlutusScriptRef (unwrap validatorV1)

          scriptRefV2 :: ScriptRef
          scriptRefV2 = PlutusScriptRef (unwrap validatorV2)

          datum :: Datum
          datum = Datum plutusData

          datum' :: Datum
          datum' = Datum plutusData'

          plutusData :: PlutusData
          plutusData = Integer $ BigInt.fromInt 31415927

          plutusData' :: PlutusData
          plutusData' =
            List
              [ Integer $ BigInt.fromInt 31415927
              , Integer $ BigInt.fromInt 7295143
              ]

          constraints0 :: TxConstraints Unit Unit
          constraints0 =
            Constraints.mustPayToPubKeyWithDatumAndScriptRef
              pkh
              datum'
              Constraints.DatumWitness
              scriptRefV1
              value
              <>
                Constraints.mustPayToPubKeyWithDatumAndScriptRef
                  pkh
                  datum
                  Constraints.DatumInline
                  scriptRefV2
                  value'
              <> Constraints.mustMintCurrency
                (mintingPolicyHash mp)
                tn
                (BigInt.fromInt 50)

        datumLookup <- liftContractM "Unable to create datum lookup" $
          Lookups.datum datum'

        let
          lookups0 :: Lookups.ScriptLookups PlutusData
          lookups0 = Lookups.mintingPolicy mp <> datumLookup

        unbalancedTx0 <-
          liftedE $ Lookups.mkUnbalancedTx lookups0 constraints0

        withBalancedTx unbalancedTx0 \balancedTx0 -> do
          balancedSignedTx0 <- signTransaction balancedTx0

          additionalUtxos <- createAdditionalUtxos balancedSignedTx0

          logInfo' $ "Additional utxos: " <> show additionalUtxos
          length additionalUtxos `shouldNotEqual` 0

          let
            constraints1 :: TxConstraints Unit Unit
            constraints1 =
              Constraints.mustPayToPubKey pkh $
                Value.lovelaceValueOf (BigInt.fromInt 60_000_000)
                  <> Value.singleton cs tn (BigInt.fromInt 50)

            lookups1 :: Lookups.ScriptLookups PlutusData
            lookups1 = Lookups.unspentOutputs additionalUtxos

            balanceTxConstraints
              :: BalanceTxConstraints.BalanceTxConstraintsBuilder
            balanceTxConstraints =
              BalanceTxConstraints.mustUseAdditionalUtxos additionalUtxos

          unbalancedTx1 <-
            liftedE $ Lookups.mkUnbalancedTx lookups1 constraints1
          balancedTx1 <-
            liftedE $ balanceTxWithConstraints unbalancedTx1
              balanceTxConstraints
          balancedSignedTx1 <- signTransaction balancedTx1

          txId0 <- submit balancedSignedTx0
          txId1 <- submit balancedSignedTx1

          awaitTxConfirmed txId0
          awaitTxConfirmed txId1

testCip30Mock :: PlutipTestM Unit
testCip30Mock = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 1_000_000_000
      , BigInt.fromInt 2_000_000_000
      ]

  Plutip.group "CIP-30 mock + Plutip" do
    withWallets distribution $ test "Wallet cleanup" \alice -> do
      try (liftEffect isNamiAvailable) >>= flip shouldSatisfy isLeft
      try (liftEffect isGeroAvailable) >>= flip shouldSatisfy isLeft
      try (liftEffect isFlintAvailable) >>= flip shouldSatisfy isLeft

      withCip30Mock alice MockNami do
        liftEffect isNamiAvailable >>= shouldEqual true
      try (liftEffect isNamiAvailable) >>= flip shouldSatisfy isLeft

      withCip30Mock alice MockGero do
        liftEffect isGeroAvailable >>= shouldEqual true
      try (liftEffect isGeroAvailable) >>= flip shouldSatisfy isLeft
      withCip30Mock alice MockFlint do
        liftEffect isFlintAvailable >>= shouldEqual true
      try (liftEffect isFlintAvailable) >>= flip shouldSatisfy isLeft

    withWallets distribution $ test "Collateral selection" \alice -> do
        withCip30Mock alice MockNami do
          getWalletCollateral >>= liftEffect <<< case _ of
            Nothing -> throw "Unable to get collateral"
            Just
              [ TransactionUnspentOutput
                  { output: TransactionOutputWithRefScript { output } }
              ] -> do
              let amount = (unwrap output).amount
              unless (amount == lovelaceValueOf (BigInt.fromInt 1_000_000_000))
                $ throw "Wrong UTxO selected as collateral"
            Just _ -> do
              -- not a bug, but unexpected
              throw "More than one UTxO in collateral"

    withWallets distribution $ test "Get own UTxOs" \alice -> do
        utxos <- withCip30Mock alice MockNami do
          getWalletUtxos
        utxos `shouldSatisfy` isJust

    withWallets distribution $ test "Get own address" \alice -> do
        mockAddress <- withCip30Mock alice MockNami do
          mbAddr <- getWalletAddress
          mbAddr `shouldSatisfy` isJust
          pure mbAddr
        kwAddress <- withKeyWallet alice do
          getWalletAddress
        mockAddress `shouldEqual` kwAddress

    withWallets distribution $ test "Pkh2Pkh" \alice -> do
        withCip30Mock alice MockNami do
          pkh <- liftedM "Failed to get PKH" ownPaymentPubKeyHash
          stakePkh <- ownStakePubKeyHash
          pkh2PkhContract pkh stakePkh

    withWallets distribution $ test "GetWalletBalance" \alice -> do
        withKeyWallet alice do
          getWalletBalance >>= flip shouldSatisfy
            ( eq $ Just $ coinToValue $ Coin $ BigInt.fromInt 1000 *
                BigInt.fromInt 3_000_000
            )
        withCip30Mock alice MockNami do
          getWalletBalance >>= flip shouldSatisfy
            ( eq $ Just $ coinToValue $ Coin $ BigInt.fromInt 1000 *
                BigInt.fromInt 3_000_000
            )

    let
      distribution' :: InitialUTxOs
      distribution' =
        [ BigInt.fromInt 2_000_000
        , BigInt.fromInt 2_000_000
        ]

    -- TODO
    withWallets distribution' $ skip $ test "Failing getWalletBalance - investigate" \alice -> do
      withCip30Mock alice MockNami do
        getWalletBalance >>= flip shouldSatisfy
          (eq $ Just $ coinToValue $ Coin $ BigInt.fromInt 3_000_000)



signMultipleContract :: forall (r :: Row Type). Contract r Unit
signMultipleContract = do
  pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  stakePkh <- ownStakePubKeyHash
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = mustPayToPubKeyStakeAddress pkh stakePkh
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  ubTx1 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  ubTx2 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints

  withBalancedTxs [ ubTx1, ubTx2 ] $ \txs -> do
    locked <- getLockedInputs
    logInfo' $ "Locked inputs inside bracket (should be nonempty): "
      <> show locked
    traverse_ (submitAndLog <=< signTransaction) txs

  locked <- getLockedInputs
  logInfo' $ "Locked inputs after bracket (should be empty): "
    <> show locked
  unless (locked # Map.isEmpty) do
    liftEffect $ throw "locked inputs map is not empty"

pkh2PkhContract
  :: forall (r :: Row Type)
   . PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> Contract r Unit
pkh2PkhContract pkh stakePkh = do
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = mustPayToPubKeyStakeAddress pkh stakePkh
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty
  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <- signTransaction =<< liftedE (balanceTx ubTx)
  submitAndLog bsTx

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
