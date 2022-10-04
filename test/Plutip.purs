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
import Contract.Chain (currentTime)
import Contract.Hashing (nativeScriptHash)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedM
  , wrapContract
  )
import Contract.PlutusData
  ( PlutusData(Bytes, Integer)
  , Redeemer(Redeemer)
  , getDatumByHash
  , getDatumsByHashes
  , getDatumsByHashesWithErrors
  )
import Contract.Prelude (mconcat)
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (applyArgs, validatorHash)
import Contract.Test.Plutip
  ( InitialUTxOs
  , runPlutipContract
  , withStakeKey
  )
import Contract.Time (getEraSummaries)
import Contract.Transaction
  ( BalancedSignedTransaction
  , DataHash
  , NativeScript(ScriptPubkey, ScriptNOfK, ScriptAll)
  , awaitTxConfirmed
  , balanceAndSignTx
  , balanceAndSignTxE
  , balanceTx
  , getTxByHash
  , signTransaction
  , submit
  , withBalancedAndSignedTxs
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
import Ctl.Examples.ContractTestUtils as ContractTestUtils
import Ctl.Examples.Helpers
  ( mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  )
import Ctl.Examples.Lose7Ada as AlwaysFails
import Ctl.Examples.MintsMultipleTokens
  ( mintingPolicyRdmrInt1
  , mintingPolicyRdmrInt2
  , mintingPolicyRdmrInt3
  )
import Ctl.Examples.OneShotMinting (contract) as OneShotMinting
import Ctl.Examples.PlutusV2.AlwaysSucceeds as AlwaysSucceedsV2
import Ctl.Examples.PlutusV2.InlineDatum as InlineDatum
import Ctl.Examples.PlutusV2.OneShotMinting (contract) as OneShotMintingV2
import Ctl.Examples.PlutusV2.ReferenceInputs (alwaysMintsPolicyV2)
import Ctl.Examples.PlutusV2.ReferenceInputs (contract) as ReferenceInputs
import Ctl.Examples.PlutusV2.ReferenceScripts (contract) as ReferenceScripts
import Ctl.Examples.SendsToken (contract) as SendsToken
import Ctl.Internal.Plutip.Server
  ( startPlutipCluster
  , startPlutipServer
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
import Ctl.Internal.Types.Interval (getSlotLength)
import Ctl.Internal.Types.UsedTxOuts (TxOutRefCache)
import Ctl.Internal.Wallet.Cip30Mock
  ( WalletMock(MockNami, MockGero, MockFlint)
  , withCip30Mock
  )
import Ctl.Internal.Wallet.Key (KeyWallet)
import Data.Array (replicate, (!!))
import Data.Bifoldable (bitraverse_)
import Data.BigInt as BigInt
import Data.Either (isLeft)
import Data.Foldable (fold, foldM)
import Data.Lens (view)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Mote (group, skip, test)
import Mote.Monad (mapTest)
import Node.ChildProcess (kill)
import Safe.Coerce (coerce)
import Test.Ctl.AffInterface as AffInterface
import Test.Ctl.Fixtures
  ( cip25MetadataFixture1
  , fullyAppliedScriptFixture
  , partiallyAppliedScriptFixture
  , unappliedScriptFixture
  )
import Test.Ctl.Plutip.Common (config, privateStakeKey)
import Test.Ctl.Plutip.Logging as Logging
import Test.Ctl.Plutip.UtxoDistribution (checkUtxoDistribution)
import Test.Ctl.Plutip.UtxoDistribution as UtxoDistribution
import Test.Ctl.TestM (TestPlanM)
import Test.Ctl.Utils as Utils
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Runner (defaultConfig)

-- Run with `spago test --main Test.Ctl.Plutip`
main :: Effect Unit
main = launchAff_ do
  Utils.interpretWithConfig
    defaultConfig { timeout = Just $ wrap 50_000.0, exit = true }
    $ do
        suite
        UtxoDistribution.suite

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Plutip" do
    Logging.suite

    test "startPlutipCluster / stopPlutipCluster" do
      bracket (startPlutipServer config)
        (bitraverse_ (liftEffect <<< kill SIGINT) identity) $ const do
        _startRes <- startPlutipCluster config [ [] ]
        stopRes <- stopPlutipCluster config
        stopRes `shouldSatisfy` case _ of
          StopClusterSuccess -> true
          _ -> false

    flip mapTest AffInterface.suite
      (runPlutipContract config unit <<< const <<< wrapContract)

    test "runPlutipContract" do
      let
        distribution :: InitialUTxOs /\ InitialUTxOs
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

    let
      arrayTest
        :: forall a
         . UtxoDistribution (Array a) (Array KeyWallet)
        => Array a
        -> Aff Unit
      arrayTest distribution = do
        runPlutipContract config distribution \wallets -> do
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
    test "runPlutipContract: Array of InitialUTxOs and KeyWallet" do
      let
        distribution :: Array InitialUTxOs
        distribution = replicate 2 [ BigInt.fromInt 1_000_000_000 ]
      arrayTest distribution

    test "runPlutipContract: Array of InitialUTxOsWithStakeKey and KeyWallet" do
      let
        distribution :: Array InitialUTxOsWithStakeKey
        distribution = withStakeKey privateStakeKey <$> replicate 2
          [ BigInt.fromInt 1_000_000_000 ]
      arrayTest distribution

    test "runPlutipContract: Pkh2Pkh" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        checkUtxoDistribution distribution alice
        pkh <- liftedM "Failed to get PKH" $ withKeyWallet alice
          ownPaymentPubKeyHash
        stakePkh <- withKeyWallet alice ownStakePubKeyHash
        withKeyWallet alice $ pkh2PkhContract pkh stakePkh

    test "runPlutipContract: Pkh2Pkh with stake key" do
      let
        aliceUtxos =
          [ BigInt.fromInt 2_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
        distribution = withStakeKey privateStakeKey aliceUtxos

      runPlutipContract config distribution \alice -> do
        checkUtxoDistribution distribution alice
        pkh <- liftedM "Failed to get PKH" $ withKeyWallet alice
          ownPaymentPubKeyHash
        stakePkh <- withKeyWallet alice ownStakePubKeyHash
        stakePkh `shouldSatisfy` isJust
        withKeyWallet alice $ pkh2PkhContract pkh stakePkh

    test "runPlutipContract: parallel Pkh2Pkh" do
      let
        aliceUtxos =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
        bobUtxos =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]

        distribution :: InitialUTxOs /\ InitialUTxOs
        distribution = aliceUtxos /\ bobUtxos

      runPlutipContract config distribution \wallets@(alice /\ bob) -> do
        checkUtxoDistribution distribution wallets
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

    test "runPlutipContract: parallel Pkh2Pkh with stake keys" do
      let
        aliceUtxos =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
        bobUtxos =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
        distribution =
          withStakeKey privateStakeKey aliceUtxos
            /\ withStakeKey privateStakeKey bobUtxos
      runPlutipContract config distribution \wallets@(alice /\ bob) ->
        do
          checkUtxoDistribution distribution wallets
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

    test "runPlutipContract: awaitTxConfirmedWithTimeout fails after timeout" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000 ]
      runPlutipContract config distribution \_ ->
        AwaitTxConfirmedWithTimeout.contract

    test "NativeScript: require all signers" do
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
      runPlutipContract config distribution \(alice /\ bob /\ charlie /\ dan) ->
        do
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
            bTx <- liftedE $ map unwrap <$> balanceTx ubTx
            tx <- liftContractM "Unable to sign transaction" =<< signTransaction
              bTx
            let
              signWithWallet txToSign wallet = withKeyWallet wallet do
                liftContractM "Unable to sign transaction" =<<
                  signTransaction txToSign
            txSigned <- foldM signWithWallet tx [ alice, bob, charlie, dan ]
            submit (wrap txSigned) >>= awaitTxConfirmed

    test "NativeScript: NOfK (2)" do
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
      runPlutipContract config distribution \(alice /\ bob /\ charlie /\ dan) ->
        do
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
            bTx <- liftedE $ map unwrap <$> balanceTx ubTx
            -- Bob signs the tx
            tx <- liftContractM "Unable to sign transaction" =<< signTransaction
              bTx
            let
              signWithWallet txToSign wallet = withKeyWallet wallet do
                liftContractM "Unable to sign transaction" =<<
                  signTransaction txToSign
            -- Dan signs the tx
            txSigned <- foldM signWithWallet tx [ dan ]
            submit (wrap txSigned) >>= awaitTxConfirmed

    test "runPlutipContract: AlwaysMints" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
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
        logInfo' <<< show =<< getDatumsByHashesWithErrors
          [ mkDatumHash
              "777093fe6dfffdb3bd2033ad71745f5e2319589e36be4bc9c8cca65ac2bfeb8f"
          , mkDatumHash
              "e8cb7d18e81b0be160c114c563c020dcc7bf148a1994b73912db3ea1318d488b"
          ]

    test "runPlutipContract: MintsMultipleTokens" do
      let
        distribution :: InitialUTxOs
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
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 100_000_000
          ]
      runPlutipContract config distribution \alice -> do
        checkUtxoDistribution distribution alice
        withKeyWallet alice signMultipleContract

    test "runPlutipContract: SignMultiple with stake key" do
      let
        aliceUtxos =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 100_000_000
          ]
        distribution = withStakeKey privateStakeKey aliceUtxos
      runPlutipContract config distribution \alice -> do
        checkUtxoDistribution distribution alice
        withKeyWallet alice signMultipleContract

    test "runPlutipContract: AlwaysSucceeds" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice do
          validator <- AlwaysSucceeds.alwaysSucceedsScript
          let vhash = validatorHash validator
          logInfo' "Attempt to lock value"
          txId <- AlwaysSucceeds.payToAlwaysSucceeds vhash
          awaitTxConfirmed txId
          logInfo' "Try to spend locked values"
          AlwaysSucceeds.spendFromAlwaysSucceeds vhash validator txId

    test "runPlutipContract: currentTime" do
      runPlutipContract config unit \_ -> do
        void $ currentTime
        void $ getEraSummaries >>= unwrap >>> traverse
          (getSlotLength >>> show >>> logInfo')

    test "runPlutipContract: SendsToken" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice SendsToken.contract

    test "runPlutipContract: InlineDatum" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice do
          validator <- InlineDatum.checkDatumIsInlineScript
          let vhash = validatorHash validator
          logInfo' "Attempt to lock value with inline datum"
          txId <- InlineDatum.payToCheckDatumIsInline vhash
          awaitTxConfirmed txId
          logInfo' "Try to spend locked values"
          InlineDatum.spendFromCheckDatumIsInline vhash validator txId

    test "runPlutipContract: InlineDatum Read" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice do
          validator <- InlineDatum.checkDatumIsInlineScript
          let vhash = validatorHash validator
          logInfo' "Attempt to lock value with inline datum"
          txId <- InlineDatum.payToCheckDatumIsInline vhash
          awaitTxConfirmed txId
          logInfo' "Try to read inline datum"
          InlineDatum.readFromCheckDatumIsInline vhash txId

    test "runPlutipContract: InlineDatum Failure" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
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

    test "runPlutipContract: InlineDatum Cannot Spend PlutusV1" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
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

    test "runPlutipContract: AlwaysSucceeds PlutusV2" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice do
          validator <- AlwaysSucceedsV2.alwaysSucceedsScriptV2
          let vhash = validatorHash validator
          logInfo' "Attempt to lock value"
          txId <- AlwaysSucceeds.payToAlwaysSucceeds vhash
          awaitTxConfirmed txId
          logInfo' "Try to spend locked values"
          AlwaysSucceeds.spendFromAlwaysSucceeds vhash validator txId

    test "runPlutipContract: AlwaysFails Ada Collateral Return" do
      let
        distribution :: InitialUTxOs /\ InitialUTxOs
        distribution =
          [ BigInt.fromInt 10_000_000
          , BigInt.fromInt 2_000_000_000
          ] /\ [ BigInt.fromInt 2_000_000_000 ]
      runPlutipContract config distribution \(alice /\ seed) -> do
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

    test "runPlutipContract: AlwaysFails Native Asset Collateral Return" do
      let
        distribution :: InitialUTxOs /\ InitialUTxOs
        distribution =
          [] /\ [ BigInt.fromInt 2_100_000_000 ]
      runPlutipContract config distribution \(alice /\ seed) -> do
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
          bsTx <- liftedE $ balanceAndSignTxE ubTx
          submit bsTx >>= awaitTxConfirmed

          logInfo' "Attempt to lock value"
          txId <- AlwaysFails.payToAlwaysFails vhash
          awaitTxConfirmed txId
          pure txId

        withKeyWallet alice do
          awaitTxConfirmed txId
          logInfo' "Try to spend locked values"
          AlwaysFails.spendFromAlwaysFails vhash validator txId

    test "runPlutipContract: ReferenceScripts" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice ->
        withKeyWallet alice ReferenceScripts.contract

    test "runPlutipContract: ReferenceInputs" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice ->
        withKeyWallet alice ReferenceInputs.contract

    test "runPlutipContract: OneShotMinting" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice ->
        withKeyWallet alice OneShotMinting.contract

    test "runPlutipContract: OneShotMinting PlutusV2" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice ->
        withKeyWallet alice OneShotMintingV2.contract

    test "runPlutipContract: Examples.ContractTestUtils" do
      let
        initialUtxos :: InitialUTxOs
        initialUtxos =
          [ BigInt.fromInt 2_000_000_000, BigInt.fromInt 2_000_000_000 ]

        distribution :: InitialUTxOs /\ InitialUTxOs
        distribution = initialUtxos /\ initialUtxos

      runPlutipContract config distribution \(alice /\ bob) -> do
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

  group "applyArgs" do
    test "returns the same script when called without args" do
      runPlutipContract config unit \_ -> do
        result <- liftedE $ applyArgs unappliedScriptFixture mempty
        result `shouldEqual` unappliedScriptFixture

    test "returns the correct partially applied Plutus script" do
      runPlutipContract config unit \_ -> do
        let args = [ Integer (BigInt.fromInt 32) ]
        result <- liftedE $ applyArgs unappliedScriptFixture args
        result `shouldEqual` partiallyAppliedScriptFixture

    test "returns the correct fully applied Plutus script" do
      runPlutipContract config unit \_ -> do
        bytes <-
          liftContractM "Could not create ByteArray" (byteArrayFromAscii "test")
        let args = [ Integer (BigInt.fromInt 32), Bytes bytes ]
        result <- liftedE $ applyArgs unappliedScriptFixture args
        result `shouldEqual` fullyAppliedScriptFixture

  group "CIP-30 mock + Plutip" do
    test "CIP-30 mock: wallet cleanup" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
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

    test "CIP-30 mock: collateral selection" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
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

    test "CIP-30 mock: get own UTxOs" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        utxos <- withCip30Mock alice MockNami do
          getWalletUtxos
        utxos `shouldSatisfy` isJust

    test "CIP-30 mock: get own address" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        mockAddress <- withCip30Mock alice MockNami do
          mbAddr <- getWalletAddress
          mbAddr `shouldSatisfy` isJust
          pure mbAddr
        kwAddress <- withKeyWallet alice do
          getWalletAddress
        mockAddress `shouldEqual` kwAddress

    test "CIP-30 mock: Pkh2Pkh" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withCip30Mock alice MockNami do
          pkh <- liftedM "Failed to get PKH" ownPaymentPubKeyHash
          stakePkh <- ownStakePubKeyHash
          pkh2PkhContract pkh stakePkh

    test "CIP-30 mock: getWalletBalance" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
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

    -- TODO
    skip $ test "CIP-30 mock: failing getWalletBalance - investigate" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 2_000_000
          , BigInt.fromInt 2_000_000
          ]
      runPlutipContract config distribution \alice -> do
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

  withBalancedAndSignedTxs [ ubTx1, ubTx2 ] $ \txs -> do
    locked <- getLockedInputs
    logInfo' $ "Locked inputs inside bracket (should be nonempty): "
      <> show locked
    traverse_ submitAndLog txs

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
  bsTx <-
    liftedE $ balanceAndSignTxE ubTx
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
