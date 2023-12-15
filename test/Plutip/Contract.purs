module Test.Ctl.Plutip.Contract
  ( suite
  ) where

import Prelude

import Contract.Address
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  , StakePubKeyHash
  , getNetworkId
  , scriptHashAddress
  )
import Contract.AuxiliaryData (setGeneralTxMetadata)
import Contract.BalanceTxConstraints
  ( BalanceTxConstraintsBuilder
  , mustUseAdditionalUtxos
  ) as BalanceTxConstraints
import Contract.BalanceTxConstraints
  ( mustNotSpendUtxosWithOutRefs
  , mustUseCollateralUtxos
  )
import Contract.Chain (currentTime, waitUntilSlot)
import Contract.Hashing (datumHash, nativeScriptHash)
import Contract.Log (logInfo')
import Contract.Metadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  , TransactionMetadatum(Text)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  )
import Contract.Monad (Contract, liftContractE, liftContractM, liftedM)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData(Bytes, Integer, List)
  , Redeemer(Redeemer)
  , getDatumByHash
  , getDatumsByHashes
  , getDatumsByHashesWithErrors
  , unitRedeemer
  )
import Contract.Prelude (liftM, mconcat)
import Contract.Prim.ByteArray
  ( byteArrayFromAscii
  , hexToByteArrayUnsafe
  , hexToRawBytes
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( ValidatorHash
  , applyArgs
  , getScriptByHash
  , getScriptsByHashes
  , mintingPolicyHash
  , validatorHash
  )
import Contract.Test (ContractTest)
import Contract.Test.Assert (runChecks)
import Contract.Test.Plutip
  ( InitialUTxOs
  , InitialUTxOsWithStakeKey
  , withStakeKey
  , withWallets
  )
import Contract.Time (Slot(Slot), getEraSummaries)
import Contract.Transaction
  ( BalanceTxError(BalanceInsufficientError, InsufficientCollateralUtxos)
  , DataHash
  , NativeScript(ScriptPubkey, ScriptNOfK, ScriptAll)
  , OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum)
  , ScriptRef(PlutusScriptRef, NativeScriptRef)
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , awaitTxConfirmed
  , balanceTx
  , balanceTxE
  , balanceTxWithConstraints
  , balanceTxWithConstraintsE
  , createAdditionalUtxos
  , getTxMetadata
  , signTransaction
  , submit
  , submitTxFromConstraints
  , withBalancedTx
  , withBalancedTxs
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx, mkUnbalancedTxE)
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (Coin(Coin), Value, coinToValue)
import Contract.Value as Value
import Contract.Wallet
  ( getWalletAddresses
  , getWalletBalance
  , getWalletCollateral
  , getWalletUtxos
  , isWalletAvailable
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  , privateKeyFromBytes
  , withKeyWallet
  )
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parallel, sequential)
import Ctl.Examples.AdditionalUtxos (contract) as AdditionalUtxos
import Ctl.Examples.AlwaysMints (alwaysMintsPolicy)
import Ctl.Examples.AlwaysSucceeds as AlwaysSucceeds
import Ctl.Examples.AwaitTxConfirmedWithTimeout as AwaitTxConfirmedWithTimeout
import Ctl.Examples.BalanceTxConstraints as BalanceTxConstraintsExample
import Ctl.Examples.Cip30 as Cip30
import Ctl.Examples.ContractTestUtils as ContractTestUtils
import Ctl.Examples.ECDSA as ECDSA
import Ctl.Examples.Helpers
  ( mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  )
import Ctl.Examples.IncludeDatum as IncludeDatum
import Ctl.Examples.Lose7Ada as AlwaysFails
import Ctl.Examples.ManyAssets as ManyAssets
import Ctl.Examples.MintsMultipleTokens
  ( mintingPolicyRdmrInt1
  , mintingPolicyRdmrInt2
  , mintingPolicyRdmrInt3
  )
import Ctl.Examples.NativeScriptMints (contract) as NativeScriptMints
import Ctl.Examples.OneShotMinting (contract) as OneShotMinting
import Ctl.Examples.PaysWithDatum (contract) as PaysWithDatum
import Ctl.Examples.PlutusV2.InlineDatum as InlineDatum
import Ctl.Examples.PlutusV2.OneShotMinting (contract) as OneShotMintingV2
import Ctl.Examples.PlutusV2.ReferenceInputs (contract) as ReferenceInputs
import Ctl.Examples.PlutusV2.ReferenceInputsAndScripts (contract) as ReferenceInputsAndScripts
import Ctl.Examples.PlutusV2.ReferenceScripts (contract) as ReferenceScripts
import Ctl.Examples.PlutusV2.Scripts.AlwaysMints (alwaysMintsPolicyV2)
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Ctl.Examples.Schnorr as Schnorr
import Ctl.Examples.SendsToken (contract) as SendsToken
import Ctl.Examples.TxChaining (contract) as TxChaining
import Ctl.Internal.Plutus.Conversion.Address (toPlutusAddress)
import Ctl.Internal.Plutus.Types.Address (Address, pubKeyHashAddress)
import Ctl.Internal.Plutus.Types.Transaction
  ( TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  , _input
  , _output
  , lookupTxHash
  )
import Ctl.Internal.Plutus.Types.Value (lovelaceValueOf)
import Ctl.Internal.Scripts (nativeScriptHashEnterpriseAddress)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.Interval (getSlotLength)
import Ctl.Internal.Wallet
  ( WalletExtension(NamiWallet, GeroWallet, FlintWallet, NuFiWallet)
  )
import Ctl.Internal.Wallet.Cip30Mock
  ( WalletMock(MockNami, MockGero, MockFlint, MockNuFi, MockGenericCip30)
  , withCip30Mock
  )
import Data.Array (head, (!!))
import Data.Either (Either(Left, Right), isLeft, isRight)
import Data.Foldable (fold, foldM, length)
import Data.Lens (view)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, isJust)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import JS.BigInt as BigInt
import Mote (group, skip, test)
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)
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
import Test.Ctl.Plutip.Common (privateStakeKey)
import Test.Ctl.Plutip.Utils (getLockedInputs, submitAndLog)
import Test.Ctl.Plutip.UtxoDistribution (checkUtxoDistribution)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite = do
  group "WaitUntilSlot" do
    test "wait for slot far in the future" do
      withWallets unit \_ -> do
        -- Plutip increases last known slot by 80 at a time
        void $ waitUntilSlot $ Slot $ BigNum.fromInt 10
        void $ waitUntilSlot $ Slot $ BigNum.fromInt 160
        void $ waitUntilSlot $ Slot $ BigNum.fromInt 161
        void $ waitUntilSlot $ Slot $ BigNum.fromInt 241
  group "Regressions" do
    skip $ test
      "#1441 - Mint many assets at once - fails with TooManyAssetsInOutput"
      do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 1000_000_000
            , BigInt.fromInt 2000_000_000
            ]
        withWallets distribution \alice -> do
          withKeyWallet alice ManyAssets.contract
    test
      "#1509 - Collateral set to one of the inputs in mustNotSpendUtxosWithOutRefs "
      do
        let
          someUtxos =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 5_000_000
            ]

        withWallets someUtxos \alice -> do
          withKeyWallet alice do
            pkh <- liftedM "Failed to get PKH" $ head <$> withKeyWallet alice
              ownPaymentPubKeyHashes
            stakePkh <- join <<< head <$> withKeyWallet alice
              ownStakePubKeyHashes
            utxos <- fromMaybe Map.empty <$> getWalletUtxos
            let
              constraints :: Constraints.TxConstraints
              constraints = mustPayToPubKeyStakeAddress pkh stakePkh
                $ Value.lovelaceValueOf
                $ BigInt.fromInt 2_000_000

              lookups :: Lookups.ScriptLookups
              lookups = mempty
            ubTx <- mkUnbalancedTx lookups constraints
            res <-
              ( balanceTxWithConstraintsE ubTx
                  (mustNotSpendUtxosWithOutRefs $ Map.keys utxos)
              )
            res `shouldSatisfy` isLeft

    test "#1480 - test that does nothing but fails" do
      let
        someUtxos =
          [ BigInt.fromInt 2_000_000
          , BigInt.fromInt 3_000_000
          ]

        privateStakeKey1 =
          wrap $ unsafePartial $ fromJust
            $ privateKeyFromBytes =<< hexToRawBytes
                "63361c4c4a075a538d37e062c1ed0706d3f0a94b013708e8f5ab0a0ca1df163d"

        privateStakeKey2 =
          wrap $ unsafePartial $ fromJust
            $ privateKeyFromBytes =<< hexToRawBytes
                "6ffb1c4c4a075a538d37e062c1ed0706d3f0a94b013708e8f5ab0a0ca1df163d"

        distribution =
          [ withStakeKey privateStakeKey someUtxos
          , withStakeKey privateStakeKey1 someUtxos
          , withStakeKey privateStakeKey2 someUtxos
          ]

      withWallets distribution \_ → pure unit

  group "Contract interface" do
    test
      "mustUseCollateralUtxos should not fail if enough UTxOs are provided"
      do
        let
          someUtxos =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 5_000_000
            ]
        withWallets (someUtxos /\ someUtxos) \(alice /\ bob) -> do
          bobsCollateral <- withKeyWallet bob do
            fromMaybe Map.empty <$> getWalletUtxos
          withKeyWallet alice do

            validator <- AlwaysSucceeds.alwaysSucceedsScript
            let vhash = validatorHash validator
            logInfo' "Attempt to lock value"
            txId <- AlwaysSucceeds.payToAlwaysSucceeds vhash
            awaitTxConfirmed txId
            logInfo' "Try to spend locked values"

            let
              scriptAddress = scriptHashAddress vhash Nothing
            utxos <- utxosAt scriptAddress
            txInput <-
              liftM
                ( error
                    ( "The id "
                        <> show txId
                        <> " does not have output locked at: "
                        <> show scriptAddress
                    )
                )
                (view _input <$> head (lookupTxHash txId utxos))
            let
              lookups :: Lookups.ScriptLookups
              lookups = Lookups.validator validator
                <> Lookups.unspentOutputs utxos

              constraints :: TxConstraints
              constraints =
                Constraints.mustSpendScriptOutput txInput unitRedeemer

            ubTx <- mkUnbalancedTx lookups constraints
            res <-
              ( balanceTxWithConstraintsE ubTx
                  $ mustUseCollateralUtxos bobsCollateral
              )
            res `shouldSatisfy` isRight

    test
      "mustUseCollateralUtxos should fail if not enough UTxOs are provided"
      do
        let
          someUtxos =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 5_000_000
            ]
        withWallets someUtxos \alice -> do
          withKeyWallet alice do

            validator <- AlwaysSucceeds.alwaysSucceedsScript
            let vhash = validatorHash validator
            logInfo' "Attempt to lock value"
            txId <- AlwaysSucceeds.payToAlwaysSucceeds vhash
            awaitTxConfirmed txId
            logInfo' "Try to spend locked values"

            let
              scriptAddress = scriptHashAddress vhash Nothing
            utxos <- utxosAt scriptAddress
            txInput <-
              liftM
                ( error
                    ( "The id "
                        <> show txId
                        <> " does not have output locked at: "
                        <> show scriptAddress
                    )
                )
                (view _input <$> head (lookupTxHash txId utxos))
            let
              lookups :: Lookups.ScriptLookups
              lookups = Lookups.validator validator
                <> Lookups.unspentOutputs utxos

              constraints :: TxConstraints
              constraints =
                Constraints.mustSpendScriptOutput txInput unitRedeemer

            ubTx <- mkUnbalancedTx lookups constraints
            res <-
              ( balanceTxWithConstraintsE ubTx
                  $ mustUseCollateralUtxos Map.empty
              )
            res `shouldSatisfy` case _ of
              Left (InsufficientCollateralUtxos mp) -> Map.isEmpty mp
              _ -> false

    test "Collateral selection: UTxO with lower amount is selected" do
      let
        distribution :: InitialUTxOs /\ InitialUTxOs
        distribution =
          [ BigInt.fromInt 10_000_000
          , BigInt.fromInt 20_000_000
          ] /\
            [ BigInt.fromInt 2_000_000_000 ]
      withWallets distribution \(alice /\ bob) -> do
        withKeyWallet alice do
          getWalletCollateral >>= liftEffect <<< case _ of
            Nothing -> throw "Unable to get collateral"
            Just
              [ TransactionUnspentOutput
                  { output: TransactionOutputWithRefScript { output } }
              ] -> do
              let amount = (unwrap output).amount
              unless (amount == lovelaceValueOf (BigInt.fromInt 10_000_000))
                $ throw "Wrong UTxO selected as collateral"
            Just _ -> do
              -- not a bug, but unexpected
              throw "More than one UTxO in collateral"
        withKeyWallet bob do
          pure unit -- sign, balance, submit, etc.

    test "Payment keyhash to payment keyhash transaction (Pkh2Pkh example)" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 10_000_000
          , BigInt.fromInt 20_000_000
          ]
      withWallets distribution \alice -> do
        checkUtxoDistribution distribution alice
        pkh <- liftedM "Failed to get PKH" $ head <$> withKeyWallet alice
          ownPaymentPubKeyHashes
        stakePkh <- join <<< head <$> withKeyWallet alice ownStakePubKeyHashes
        withKeyWallet alice $ pkh2PkhContract pkh stakePkh

    test
      "Base Address to Base Address transaction (Pkh2Pkh example, but with stake keys)"
      do
        let
          aliceUtxos =
            [ BigInt.fromInt 20_000_000
            , BigInt.fromInt 20_000_000
            ]
          distribution = withStakeKey privateStakeKey aliceUtxos

        withWallets distribution \alice -> do
          checkUtxoDistribution distribution alice
          pkh <- liftedM "Failed to get PKH" $ head <$> withKeyWallet alice
            ownPaymentPubKeyHashes
          stakePkh <- join <<< head <$> withKeyWallet alice
            ownStakePubKeyHashes
          stakePkh `shouldSatisfy` isJust
          withKeyWallet alice $ pkh2PkhContract pkh stakePkh

    test
      "Payment key hash to payment key hash Tx: running two contracts in parallel (Pkh2Pkh example)"
      do
        let
          aliceUtxos =
            [ BigInt.fromInt 20_000_000
            , BigInt.fromInt 20_000_000
            ]
          bobUtxos =
            [ BigInt.fromInt 20_000_000
            , BigInt.fromInt 20_000_000
            ]

          distribution :: InitialUTxOs /\ InitialUTxOs
          distribution = aliceUtxos /\ bobUtxos

        withWallets distribution \wallets@(alice /\ bob) -> do
          checkUtxoDistribution distribution wallets
          sequential ado
            parallel $ withKeyWallet alice do
              pkh <- liftedM "Failed to get PKH" $ head <$> withKeyWallet bob
                ownPaymentPubKeyHashes
              stakePkh <- join <<< head <$> withKeyWallet bob
                ownStakePubKeyHashes
              pkh2PkhContract pkh stakePkh
            parallel $ withKeyWallet bob do
              pkh <- liftedM "Failed to get PKH" $ head <$> withKeyWallet alice
                ownPaymentPubKeyHashes
              stakePkh <- join <<< head <$> withKeyWallet alice
                ownStakePubKeyHashes
              pkh2PkhContract pkh stakePkh
            in unit

    test
      "Base Address to Base Address hash Tx: running two contracts in parallel (Pkh2Pkh example)"
      do
        let
          aliceUtxos =
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 20_000_000
            ]
          bobUtxos =
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 20_000_000
            ]
          distribution =
            withStakeKey privateStakeKey aliceUtxos
              /\ withStakeKey privateStakeKey bobUtxos
        withWallets distribution \wallets@(alice /\ bob) ->
          do
            checkUtxoDistribution distribution wallets
            sequential ado
              parallel $ withKeyWallet alice do
                pkh <- liftedM "Failed to get PKH" $ head <$> withKeyWallet bob
                  ownPaymentPubKeyHashes
                stakePkh <- join <<< head <$> withKeyWallet bob
                  ownStakePubKeyHashes
                pkh2PkhContract pkh stakePkh
              parallel $ withKeyWallet bob do
                pkh <- liftedM "Failed to get PKH" $ head <$> withKeyWallet
                  alice
                  ownPaymentPubKeyHashes
                stakePkh <- join <<< head <$> withKeyWallet alice
                  ownStakePubKeyHashes
                pkh2PkhContract pkh stakePkh
              in unit

    test "Tx confirmation fails after timeout (awaitTxConfirmedWithTimeout)" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000 ]
      withWallets distribution \_ ->
        AwaitTxConfirmedWithTimeout.contract

    test "NativeScript (multisig) support: require all signers" do
      let
        distribution
          :: InitialUTxOs /\ InitialUTxOs /\ InitialUTxOs /\ InitialUTxOs
        distribution =
          [ BigInt.fromInt 20_000_000
          , BigInt.fromInt 20_000_000
          ]
            /\
              [ BigInt.fromInt 20_000_000
              , BigInt.fromInt 20_000_000
              ]
            /\
              [ BigInt.fromInt 20_000_000
              , BigInt.fromInt 20_000_000
              ]
            /\
              [ BigInt.fromInt 20_000_000
              , BigInt.fromInt 20_000_000
              ]
      withWallets distribution \(alice /\ bob /\ charlie /\ dan) ->
        do
          alicePaymentPKH <- liftedM "Unable to get Alice's PKH" $
            (coerce <<< head) <$> withKeyWallet alice ownPaymentPubKeyHashes
          bobPaymentPKH <- liftedM "Unable to get Bob's PKH" $
            (coerce <<< head) <$> withKeyWallet bob ownPaymentPubKeyHashes
          charliePaymentPKH <- liftedM "Unable to get Charlie's PKH" $
            (coerce <<< head) <$> withKeyWallet charlie
              ownPaymentPubKeyHashes
          danPaymentPKH <- liftedM "Unable to get Dan's PKH" $
            (coerce <<< head) <$> withKeyWallet dan ownPaymentPubKeyHashes
          let
            nativeScript = ScriptAll
              [ ScriptPubkey alicePaymentPKH
              , ScriptPubkey bobPaymentPKH
              , ScriptPubkey charliePaymentPKH
              , ScriptPubkey danPaymentPKH
              ]
            nsHash = nativeScriptHash nativeScript

          -- Alice locks 10 ADA at mutlisig script
          txId <- withKeyWallet alice do
            let
              constraints :: TxConstraints
              constraints = Constraints.mustPayToNativeScript nsHash
                $ Value.lovelaceValueOf
                $ BigInt.fromInt 10_000_000

              lookups :: Lookups.ScriptLookups
              lookups = mempty

            ubTx <- mkUnbalancedTx lookups constraints
            bsTx <- signTransaction =<< balanceTx ubTx
            txId <- submit bsTx
            awaitTxConfirmed txId
            pure txId
          -- Bob attempts to unlock and send Ada to Charlie
          withKeyWallet bob do
            -- First, he should find the transaction input where Ada is locked
            networkId <- getNetworkId
            let
              nsAddr = nativeScriptHashEnterpriseAddress networkId nsHash
            nsAddrPlutus <- liftContractM "Unable to convert to Plutus address"
              $ toPlutusAddress nsAddr
            utxos <- utxosAt nsAddrPlutus
            txInput <- liftContractM "Unable to get UTxO" $
              view _input <$> lookupTxHash txId utxos !! 0
            let
              constraints :: TxConstraints
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

              lookups :: Lookups.ScriptLookups
              lookups = Lookups.unspentOutputs utxos

            ubTx <- mkUnbalancedTx lookups constraints
            tx <- signTransaction =<< balanceTx ubTx
            let
              signWithWallet txToSign wallet =
                withKeyWallet wallet (signTransaction txToSign)
            txSigned <- foldM signWithWallet tx [ alice, bob, charlie, dan ]
            submit txSigned >>= awaitTxConfirmed

    test "NativeScript support: require N=2 of K=4 signers" do
      let
        distribution
          :: InitialUTxOs /\ InitialUTxOs /\ InitialUTxOs /\ InitialUTxOs
        distribution =
          [ BigInt.fromInt 50_000_000
          , BigInt.fromInt 50_000_000
          ]
            /\
              [ BigInt.fromInt 50_000_000
              , BigInt.fromInt 50_000_000
              ]
            /\
              [ BigInt.fromInt 50_000_000
              , BigInt.fromInt 50_000_000
              ]
            /\
              [ BigInt.fromInt 50_000_000
              , BigInt.fromInt 50_000_000
              ]
      withWallets distribution \(alice /\ bob /\ charlie /\ dan) ->
        do
          alicePaymentPKH <- liftedM "Unable to get Alice's PKH" $
            (coerce <<< head) <$> withKeyWallet alice ownPaymentPubKeyHashes
          bobPaymentPKH <- liftedM "Unable to get Bob's PKH" $
            (coerce <<< head) <$> withKeyWallet bob ownPaymentPubKeyHashes
          charliePaymentPKH <- liftedM "Unable to get Charlie's PKH" $
            (coerce <<< head) <$> withKeyWallet charlie
              ownPaymentPubKeyHashes
          danPaymentPKH <- liftedM "Unable to get Dan's PKH" $
            (coerce <<< head) <$> withKeyWallet dan ownPaymentPubKeyHashes
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
              constraints :: TxConstraints
              constraints = Constraints.mustPayToNativeScript nsHash
                $ Value.lovelaceValueOf
                $ BigInt.fromInt 10_000_000

              lookups :: Lookups.ScriptLookups
              lookups = mempty

            ubTx <- mkUnbalancedTx lookups constraints
            bsTx <- signTransaction =<< balanceTx ubTx
            txId <- submit bsTx
            awaitTxConfirmed txId
            pure txId

          -- Bob attempts to unlock and send Ada to Charlie
          withKeyWallet bob do
            -- First, he should find the transaction input where Ada is locked
            networkId <- getNetworkId
            let
              nsAddr = nativeScriptHashEnterpriseAddress networkId nsHash
            nsAddrPlutus <- liftContractM "Unable to convert to Plutus address"
              $ toPlutusAddress nsAddr
            utxos <- utxosAt nsAddrPlutus
            txInput <- liftContractM "Unable to get UTxO" $
              view _input <$> lookupTxHash txId utxos !! 0
            let
              constraints :: TxConstraints
              constraints =
                Constraints.mustPayToPubKey (coerce alicePaymentPKH)
                  (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)
                  <> Constraints.mustSpendNativeScriptOutput txInput
                    nativeScript

              lookups :: Lookups.ScriptLookups
              lookups = Lookups.unspentOutputs utxos

            ubTx <- mkUnbalancedTx lookups constraints
            -- Bob signs the tx
            tx <- signTransaction =<< balanceTx ubTx
            let
              signWithWallet txToSign wallet =
                withKeyWallet wallet (signTransaction txToSign)
            -- Dan signs the tx
            txSigned <- foldM signWithWallet tx [ dan ]
            submit txSigned >>= awaitTxConfirmed

    test "An always-succeeding minting policy" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          mp <- alwaysMintsPolicy
          let cs = Value.scriptCurrencySymbol mp
          tn <- liftContractM "Cannot make token name"
            $ Value.mkTokenName
                =<< byteArrayFromAscii "TheToken"

          let
            constraints :: Constraints.TxConstraints
            constraints = Constraints.mustMintValue
              $ Value.singleton cs tn
              $ BigInt.fromInt 100

            lookups :: Lookups.ScriptLookups
            lookups = Lookups.mintingPolicy mp

          ubTx <- mkUnbalancedTx lookups constraints
          bsTx <- signTransaction =<< balanceTx ubTx
          submitAndLog bsTx

    test "mustProduceAtLeast spends native token" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          mp <- alwaysMintsPolicy
          _ /\ cs <- mkCurrencySymbol alwaysMintsPolicy
          tn <- mkTokenName "TheToken"

          -- Minting

          let
            constraints :: Constraints.TxConstraints
            constraints = Constraints.mustMintValue
              $ Value.singleton cs tn
              $ BigInt.fromInt 100

            lookups :: Lookups.ScriptLookups
            lookups = Lookups.mintingPolicy mp

          txHash <- submitTxFromConstraints lookups constraints
          awaitTxConfirmed txHash

          -- Spending same amount

          pkh <-
            liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes

          let
            constraints' :: Constraints.TxConstraints
            constraints' = Constraints.mustProduceAtLeast
              $ Value.singleton cs tn
              $ BigInt.fromInt 100
            lookups' = lookups <> Lookups.ownPaymentPubKeyHash pkh

          txHash' <- submitTxFromConstraints lookups' constraints'
          void $ awaitTxConfirmed txHash'

    test "mustProduceAtLeast fails to produce more tokens than there is" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          mp <- alwaysMintsPolicy
          _ /\ cs <- mkCurrencySymbol alwaysMintsPolicy
          tn <- mkTokenName "TheToken"

          -- Minting

          let
            constraints :: Constraints.TxConstraints
            constraints = Constraints.mustMintValue
              $ Value.singleton cs tn
              $ BigInt.fromInt 100

            lookups :: Lookups.ScriptLookups
            lookups = Lookups.mintingPolicy mp

          txHash <- submitTxFromConstraints lookups constraints
          awaitTxConfirmed txHash

          -- Spending more than minted amount

          pkh <-
            liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes

          let
            constraints' :: Constraints.TxConstraints
            constraints' = Constraints.mustProduceAtLeast
              $ Value.singleton cs tn
              $ BigInt.fromInt 101
            lookups' = lookups <> Lookups.ownPaymentPubKeyHash pkh

          ubTx <- mkUnbalancedTx lookups' constraints'
          result <- balanceTxE ubTx
          result `shouldSatisfy` isLeft

    test "mustSpendAtLeast succeeds to spend" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          mp <- alwaysMintsPolicy
          _ /\ cs <- mkCurrencySymbol alwaysMintsPolicy
          tn <- mkTokenName "TheToken"

          -- Minting

          let
            constraints :: Constraints.TxConstraints
            constraints = Constraints.mustMintValue
              $ Value.singleton cs tn
              $ BigInt.fromInt 100

            lookups :: Lookups.ScriptLookups
            lookups = Lookups.mintingPolicy mp

          txHash <- submitTxFromConstraints lookups constraints
          awaitTxConfirmed txHash

          -- Spending same amount

          pkh <-
            liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes

          let
            constraints' :: Constraints.TxConstraints
            constraints' = Constraints.mustSpendAtLeast
              $ Value.singleton cs tn
              $ BigInt.fromInt 100
            lookups' = lookups <> Lookups.ownPaymentPubKeyHash pkh

          txHash' <- submitTxFromConstraints lookups' constraints'
          void $ awaitTxConfirmed txHash'

    test "mustSpendAtLeast fails to spend more token that there is" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          mp <- alwaysMintsPolicy
          _ /\ cs <- mkCurrencySymbol alwaysMintsPolicy
          tn <- mkTokenName "TheToken"

          -- Minting

          let
            constraints :: Constraints.TxConstraints
            constraints = Constraints.mustMintValue
              $ Value.singleton cs tn
              $ BigInt.fromInt 100

            lookups :: Lookups.ScriptLookups
            lookups = Lookups.mintingPolicy mp

          txHash <- submitTxFromConstraints lookups constraints
          awaitTxConfirmed txHash

          -- Spending more than minted amount

          pkh <-
            liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes

          let
            constraints' :: Constraints.TxConstraints
            constraints' = Constraints.mustSpendAtLeast
              $ Value.singleton cs tn
              $ BigInt.fromInt 101
            lookups' = lookups <> Lookups.ownPaymentPubKeyHash pkh

          ubTx <- mkUnbalancedTx lookups' constraints'
          result <- balanceTxE ubTx
          result `shouldSatisfy` isLeft

    test "Minting using NativeScript (multisig) as a policy" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice NativeScriptMints.contract

    test "Getting datums by hashes" do
      withWallets unit \_ -> do
        let
          mkDatumHash :: String -> DataHash
          mkDatumHash = wrap <<< hexToByteArrayUnsafe
        -- Nothing is expected, because we are in an empty chain.
        -- This test only checks for ability to connect to the datum-querying
        -- backend.
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

    test "GetDatumsByHashes" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]

        datum1 :: Datum
        datum1 = Datum $ Integer $ BigInt.fromInt 1

        datum2 :: Datum
        datum2 = Datum $ Integer $ BigInt.fromInt 2

        datums :: Array Datum
        datums = [ datum2, datum1 ]

      let
        payToTest :: ValidatorHash -> Contract TransactionHash
        payToTest vhash = do
          let
            constraints =
              Constraints.mustPayToScript
                vhash
                datum1
                Constraints.DatumWitness
                (Value.lovelaceValueOf $ BigInt.fromInt 1_000_000)
                <> Constraints.mustPayToScript
                  vhash
                  datum2
                  Constraints.DatumWitness
                  (Value.lovelaceValueOf $ BigInt.fromInt 1_000_000)

            lookups :: Lookups.ScriptLookups
            lookups = mempty
          submitTxFromConstraints lookups constraints

      withWallets distribution \alice -> do
        withKeyWallet alice do
          validator <- AlwaysSucceeds.alwaysSucceedsScript
          let vhash = validatorHash validator
          logInfo' "Running GetDatums submittx"
          txId <- payToTest vhash
          awaitTxConfirmed txId
          logInfo' "Tx submitted successfully, trying to fetch datum"

          let
            hash1 = datumHash datum1
            hash2 = datumHash datum2
            hashes = map datumHash datums

          actualDatums1 <- getDatumsByHashes hashes
          actualDatums1 `shouldEqual`
            ( Map.fromFoldable
                [ hash1 /\ datum1
                , hash2 /\ datum2
                ]
            )
          actualDatums2 <- getDatumsByHashesWithErrors hashes
          actualDatums2 `shouldEqual`
            ( Map.fromFoldable
                [ hash1 /\ Right datum1
                , hash2 /\ Right datum2
                ]
            )

    test "GetScriptByHash" do
      let
        distribution :: InitialUTxOs
        distribution = [ BigInt.fromInt 50_000_000 ]

      withWallets distribution \alice -> do
        withKeyWallet alice do
          validator1 <- AlwaysSucceeds.alwaysSucceedsScript
          validator2 <- alwaysSucceedsScriptV2
          let
            validatorRef1 = PlutusScriptRef $ unwrap validator1
            validatorRef2 = PlutusScriptRef $ unwrap validator2
            useScriptAndGetByHash validator vhash = do
              txId <- AlwaysSucceeds.payToAlwaysSucceeds vhash
              awaitTxConfirmed txId
              -- Spending utxo, to make Kupo (used inside) see the script
              AlwaysSucceeds.spendFromAlwaysSucceeds vhash validator txId
              getScriptByHash $ unwrap vhash

          result1 <- useScriptAndGetByHash validator1 (validatorHash validator1)
          result2 <- useScriptAndGetByHash validator2 (validatorHash validator2)

          -- Testing getScriptByHash
          result1 `shouldEqual` (Right (Just validatorRef1))
          result2 `shouldEqual` (Right (Just validatorRef2))

          -- Testing getScriptsByHashes
          let
            scriptHash1 = unwrap (validatorHash validator1)
            scriptHash2 = unwrap (validatorHash validator2)
          results <- getScriptsByHashes [ scriptHash1, scriptHash2 ]
          results `shouldEqual` Map.fromFoldable
            [ (scriptHash1 /\ (Right (Just validatorRef1)))
            , (scriptHash2 /\ (Right (Just validatorRef2)))
            ]

    test "Getting transaction metadata" do
      let
        distribution :: InitialUTxOs
        distribution = [ BigInt.fromInt 50_000_000 ]

      withWallets distribution \alice -> do
        withKeyWallet alice do
          let
            constraints :: Constraints.TxConstraints
            constraints = mempty

            lookups :: Lookups.ScriptLookups
            lookups = mempty
            givenMetadata = GeneralTransactionMetadata $ Map.fromFoldable
              [ TransactionMetadatumLabel (BigInt.fromInt 8) /\ Text "foo" ]

          ubTx <- mkUnbalancedTx lookups constraints
          ubTx' <- setGeneralTxMetadata ubTx givenMetadata
          bsTx <- signTransaction =<< balanceTx ubTx'
          txId <- submit bsTx
          awaitTxConfirmed txId

          mMetadata <- getTxMetadata txId
          mMetadata `shouldEqual` Right givenMetadata

    test "Minting zero of a token fails" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]

      withWallets distribution \alice -> do
        withKeyWallet alice do
          tn1 <- mkTokenName "Token name"
          mp1 /\ _ <- mkCurrencySymbol alwaysMintsPolicy
          mp2 /\ _ <- mkCurrencySymbol alwaysMintsPolicyV2

          let
            constraints :: Constraints.TxConstraints
            constraints = mconcat
              [ Constraints.mustMintCurrency (mintingPolicyHash mp1) tn1 zero
              , Constraints.mustMintCurrency (mintingPolicyHash mp2) tn1 one
              ]

            lookups :: Lookups.ScriptLookups
            lookups =
              Lookups.mintingPolicy mp1 <> Lookups.mintingPolicy mp2
          result <- mkUnbalancedTxE lookups constraints
          result `shouldSatisfy` isLeft

    test "Minting multiple tokens in a single transaction" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          tn1 <- mkTokenName "Token with a long name"
          tn2 <- mkTokenName "Token"
          mp1 /\ cs1 <- mkCurrencySymbol mintingPolicyRdmrInt1
          mp2 /\ cs2 <- mkCurrencySymbol mintingPolicyRdmrInt2
          mp3 /\ cs3 <- mkCurrencySymbol mintingPolicyRdmrInt3

          let
            constraints :: Constraints.TxConstraints
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

            lookups :: Lookups.ScriptLookups
            lookups =
              Lookups.mintingPolicy mp1
                <> Lookups.mintingPolicy mp2
                <> Lookups.mintingPolicy mp3

          ubTx <- mkUnbalancedTx lookups constraints
          bsTx <- signTransaction =<< balanceTx ubTx
          submitAndLog bsTx

    test "Multi-signature transaction" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        checkUtxoDistribution distribution alice
        withKeyWallet alice signMultipleContract

    test "Multi-signature transaction with BaseAddresses" do
      let
        aliceUtxos =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
        distribution = withStakeKey privateStakeKey aliceUtxos
      withWallets distribution \alice -> do
        checkUtxoDistribution distribution alice
        withKeyWallet alice signMultipleContract

    test "AdditionalUtxos example" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 10_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice ->
        withKeyWallet alice $ AdditionalUtxos.contract false

    test "Handles AdditionalUtxoOverlap exception (AdditionalUtxos example)" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 10_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice ->
        withKeyWallet alice $ AdditionalUtxos.contract true

    test
      "Locking & unlocking on an always succeeding script (AlwaysSucceeds example)"
      do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          withKeyWallet alice do
            validator <- AlwaysSucceeds.alwaysSucceedsScript
            let vhash = validatorHash validator
            logInfo' "Attempt to lock value"
            txId <- AlwaysSucceeds.payToAlwaysSucceeds vhash
            awaitTxConfirmed txId
            logInfo' "Try to spend locked values"
            AlwaysSucceeds.spendFromAlwaysSucceeds vhash validator txId

    test
      "AlwaysSucceeds example (with stake key to test `mustPayToPubKeyAddress`)"
      do
        let
          distribution :: InitialUTxOsWithStakeKey
          distribution = withStakeKey privateStakeKey
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          withKeyWallet alice do
            validator <- AlwaysSucceeds.alwaysSucceedsScript
            let vhash = validatorHash validator
            logInfo' "Attempt to lock value"
            txId <- AlwaysSucceeds.payToAlwaysSucceeds vhash
            awaitTxConfirmed txId
            logInfo' "Try to spend locked values"
            AlwaysSucceeds.spendFromAlwaysSucceeds vhash validator txId

    test "Query for current time and era summaries" do
      withWallets unit \_ -> do
        void $ currentTime
        void $ getEraSummaries >>= unwrap >>> traverse
          (getSlotLength >>> show >>> logInfo')

    test "Mints and sends a token" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice SendsToken.contract

    test "PlutusV1 forces balancer to select non-PlutusV2 inputs" do
      let
        distribution :: InitialUTxOs
        distribution = [ BigInt.fromInt 5_000_000 ]

      withWallets distribution \alice -> do
        alicePkh <- withKeyWallet alice do
          liftedM "Could not get own PKH" (head <$> ownPaymentPubKeyHashes)

        validator <- AlwaysSucceeds.alwaysSucceedsScript

        let
          vhash = validatorHash validator
          scriptAddress = scriptHashAddress vhash Nothing

        let
          datum42 = Datum $ Integer $ BigInt.fromInt 42
          datum42Hash = datumHash datum42
          datum42Lookup = Lookups.datum datum42

        let
          transactionId :: TransactionHash
          transactionId = TransactionHash $ hexToByteArrayUnsafe
            "a6b656487601c390a3bb61958c62369cb5d5a7597a68a9dccedb3dd68a60bfdd"

          mkUtxo
            :: UInt
            -> Address
            -> Value
            -> OutputDatum
            -> TransactionUnspentOutput
          mkUtxo index address amount datum =
            TransactionUnspentOutput
              { input: TransactionInput
                  { index
                  , transactionId
                  }
              , output: TransactionOutputWithRefScript
                  { scriptRef: Nothing
                  , output: TransactionOutput
                      { address
                      , amount
                      , datum
                      , referenceScript: Nothing
                      }
                  }
              }

          aliceUtxo :: OutputDatum -> TransactionUnspentOutput
          aliceUtxo = mkUtxo zero
            (pubKeyHashAddress alicePkh Nothing)
            (Value.lovelaceValueOf $ BigInt.fromInt 50_000_000)

          alwaysSucceedsUtxo :: TransactionUnspentOutput
          alwaysSucceedsUtxo = mkUtxo one
            scriptAddress
            (Value.lovelaceValueOf $ BigInt.fromInt 2_000_000)
            (OutputDatumHash datum42Hash)

          -- Balance a transaction which requires selecting a utxo with a
          -- certain datum
          balanceWithDatum datum = withKeyWallet alice do
            let
              additionalUtxos :: UtxoMap
              additionalUtxos =
                Map.fromFoldable $ (Tuple <$> view _input <*> view _output) <$>
                  [ alwaysSucceedsUtxo, aliceUtxo datum ]

              value :: Value.Value
              value = Value.lovelaceValueOf $ BigInt.fromInt 50_000_000

              constraints :: TxConstraints
              constraints = fold
                [ Constraints.mustSpendScriptOutput
                    (view _input alwaysSucceedsUtxo)
                    unitRedeemer
                , Constraints.mustPayToPubKey alicePkh value
                ]

              lookups :: Lookups.ScriptLookups
              lookups =
                Lookups.validator validator
                  <> Lookups.unspentOutputs additionalUtxos
                  <> datum42Lookup

              balanceTxConstraints
                :: BalanceTxConstraints.BalanceTxConstraintsBuilder
              balanceTxConstraints =
                BalanceTxConstraints.mustUseAdditionalUtxos additionalUtxos

            unbalancedTx <- mkUnbalancedTx lookups constraints
            balanceTxWithConstraintsE unbalancedTx balanceTxConstraints

        let
          hasInsufficientBalance
            :: forall (a :: Type). Either BalanceTxError a -> Boolean
          hasInsufficientBalance = case _ of
            Left (BalanceInsufficientError _ _) -> true
            _ -> false

        balanceWithDatum NoOutputDatum >>= flip shouldSatisfy isRight
        balanceWithDatum (OutputDatum datum42) >>= flip shouldSatisfy
          hasInsufficientBalance

    test "InlineDatum" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          validator <- InlineDatum.checkDatumIsInlineScript
          let vhash = validatorHash validator
          logInfo' "Attempt to lock value with inline datum"
          txId <- InlineDatum.payToCheckDatumIsInline vhash
          awaitTxConfirmed txId
          logInfo' "Try to spend locked values"
          InlineDatum.spendFromCheckDatumIsInline vhash validator txId

    group "CIP-32 InlineDatums" do
      test "Use of CIP-32 InlineDatums" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          withKeyWallet alice do
            validator <- InlineDatum.checkDatumIsInlineScript
            let vhash = validatorHash validator
            logInfo' "Attempt to lock value with inline datum"
            txId <- InlineDatum.payToCheckDatumIsInline vhash
            awaitTxConfirmed txId
            logInfo' "Try to spend locked values"
            InlineDatum.spendFromCheckDatumIsInline vhash validator txId

      test "Use of CIP-30 InlineDatums without spending the UTxO (readonly)" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          withKeyWallet alice do
            validator <- InlineDatum.checkDatumIsInlineScript
            let vhash = validatorHash validator
            logInfo' "Attempt to lock value with inline datum"
            txId <- InlineDatum.payToCheckDatumIsInline vhash
            awaitTxConfirmed txId
            logInfo' "Try to read inline datum"
            InlineDatum.readFromCheckDatumIsInline vhash txId

      test "InlineDatum spending fails because the datum was not set inline" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
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

      test "InlineDatum fails because PlutusV1 script is used" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          withKeyWallet alice do
            validator <- AlwaysSucceeds.alwaysSucceedsScript
            let vhash = validatorHash validator
            logInfo'
              "Attempt to lock value at plutusv1 script with inline datum"
            txId <- InlineDatum.payToCheckDatumIsInline vhash
            awaitTxConfirmed txId
            logInfo' "Try to spend locked values"
            eResult <- try $ InlineDatum.spendFromCheckDatumIsInline vhash
              validator
              txId
            eResult `shouldSatisfy` isLeft

      test "Payment with inline datum" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice ->
          withKeyWallet alice PaysWithDatum.contract

    test "Lock value at a script: validator that only accepts 42 as redeemer" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          validator <- IncludeDatum.only42Script
          let vhash = validatorHash validator
          logInfo' "Attempt to lock value"
          txId <- IncludeDatum.payToIncludeDatum vhash
          awaitTxConfirmed txId
          logInfo' "Try to spend locked values"
          IncludeDatum.spendFromIncludeDatum vhash validator txId

    test "Always succeeding PlutusV2 script" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          validator <- alwaysSucceedsScriptV2
          let vhash = validatorHash validator
          logInfo' "Attempt to lock value"
          txId <- AlwaysSucceeds.payToAlwaysSucceeds vhash
          awaitTxConfirmed txId
          logInfo' "Try to spend locked values"
          AlwaysSucceeds.spendFromAlwaysSucceeds vhash validator txId

    group "CIP-40 Collateral Output" do
      test "Always failing script triggers Collateral Return (ADA-only)" do
        let
          distribution :: InitialUTxOs /\ InitialUTxOs
          distribution =
            [ BigInt.fromInt 10_000_000
            , BigInt.fromInt 50_000_000
            ] /\ [ BigInt.fromInt 50_000_000 ]
        withWallets distribution \(alice /\ seed) -> do
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
              collateralLoss = Value.lovelaceValueOf $ BigInt.fromInt $
                -5_000_000
            balance `shouldEqual` (balanceBefore <> collateralLoss)

      test "AlwaysFails script triggers Native Asset Collateral Return (tokens)"
        do
          let
            distribution :: InitialUTxOs /\ InitialUTxOs
            distribution =
              [] /\ [ BigInt.fromInt 2_100_000_000 ]
          withWallets distribution \(alice /\ seed) -> do
            alicePkh /\ aliceStakePkh <- withKeyWallet alice do
              pkh <- liftedM "Failed to get PKH" $ head <$>
                ownPaymentPubKeyHashes
              stakePkh <- join <<< head <$> ownStakePubKeyHashes
              pure $ pkh /\ stakePkh

            mp <- alwaysMintsPolicy
            let cs = Value.scriptCurrencySymbol mp
            tn <- liftContractM "Cannot make token name"
              $ byteArrayFromAscii "TheToken" >>= Value.mkTokenName
            let asset = Value.singleton cs tn $ BigInt.fromInt 50

            validator <- AlwaysFails.alwaysFailsScript
            let vhash = validatorHash validator

            txId <- withKeyWallet seed do
              logInfo' "Minting asset to Alice"
              let
                constraints :: Constraints.TxConstraints
                constraints = Constraints.mustMintValue (asset <> asset)
                  <> mustPayToPubKeyStakeAddress alicePkh aliceStakePkh
                    ( asset <>
                        (Value.lovelaceValueOf $ BigInt.fromInt 10_000_000)
                    )
                  <> mustPayToPubKeyStakeAddress alicePkh aliceStakePkh
                    ( asset <>
                        (Value.lovelaceValueOf $ BigInt.fromInt 50_000_000)
                    )

                lookups :: Lookups.ScriptLookups
                lookups = Lookups.mintingPolicy mp

              ubTx <- mkUnbalancedTx lookups constraints
              bsTx <- signTransaction =<< balanceTx ubTx
              submit bsTx >>= awaitTxConfirmed

              logInfo' "Attempt to lock value"
              txId <- AlwaysFails.payToAlwaysFails vhash
              awaitTxConfirmed txId
              pure txId

            withKeyWallet alice do
              awaitTxConfirmed txId
              logInfo' "Try to spend locked values"
              AlwaysFails.spendFromAlwaysFails vhash validator txId

    group "CIP-33 Reference Scripts" do
      test "Use reference scripts for spending" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice ->
          withKeyWallet alice ReferenceScripts.contract

      test
        "Use reference scripts for spending (with Base Address, testing `mustPayToScriptAddressWithScriptRef`)"
        do
          let
            distribution :: InitialUTxOsWithStakeKey
            distribution = withStakeKey privateStakeKey
              [ BigInt.fromInt 5_000_000
              , BigInt.fromInt 50_000_000
              ]
          withWallets distribution \alice ->
            withKeyWallet alice ReferenceScripts.contract

    group "CIP-31 Reference Inputs" do
      test "Use reference inputs" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice ->
          withKeyWallet alice ReferenceInputs.contract

      test "Use reference inputs and reference scripts at the same time" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice ->
          withKeyWallet alice ReferenceInputsAndScripts.contract

    test "One-Shot Minting example" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice ->
        withKeyWallet alice OneShotMinting.contract

    test "One-Shot Minting using PlutusV2 scripts" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice ->
        withKeyWallet alice OneShotMintingV2.contract

    test "Check assertion utilities (ContractTestUtils example)" do
      let
        initialUtxos :: InitialUTxOs
        initialUtxos =
          [ BigInt.fromInt 50_000_000, BigInt.fromInt 50_000_000 ]

        distribution :: InitialUTxOs /\ InitialUTxOs
        distribution = initialUtxos /\ initialUtxos

      withWallets distribution \(alice /\ bob) -> do
        receiverPkh <- liftedM "Unable to get Bob's PKH" $
          head <$> withKeyWallet bob ownPaymentPubKeyHashes
        receiverSkh <- join <<< head <$> withKeyWallet bob ownStakePubKeyHashes

        mintingPolicy /\ cs <- mkCurrencySymbol alwaysMintsPolicyV2

        tn <- mkTokenName "TheToken"

        withKeyWallet alice do
          let
            params =
              { receiverPkh
              , receiverSkh
              , adaToSend: BigInt.fromInt 5_000_000
              , mintingPolicy
              , tokensToMint: cs /\ tn /\ one /\ unit
              , datumToAttach: wrap $ Integer $ BigInt.fromInt 42
              , txMetadata: cip25MetadataFixture1
              }

          checks <- ContractTestUtils.mkChecks params
          void $ runChecks checks $ lift do
            ContractTestUtils.mkContract params

    test "Transaction balancer constraints (BalanceTxConstraints example)" do
      let
        initialUtxos :: InitialUTxOs
        initialUtxos =
          [ BigInt.fromInt 50_000_000, BigInt.fromInt 50_000_000 ]

        distribution :: InitialUTxOs /\ InitialUTxOs
        distribution = initialUtxos /\ initialUtxos

      withWallets distribution \(alice /\ bob) ->
        withKeyWallet alice $ BalanceTxConstraintsExample.contract $
          BalanceTxConstraintsExample.ContractParams
            { aliceKeyWallet: alice, bobKeyWallet: bob }

    -- FIXME These tests never require additionalUtxos to succeed. They do
    -- not invoke a script.
    -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1392
    group "Evaluation with additional UTxOs and Tx chaining" do
      test "Tx chain submits (TxChaining example)" $
        let
          distribution :: InitialUTxOs
          distribution = [ BigInt.fromInt 2_500_000 ]
        in
          withWallets distribution \alice ->
            withKeyWallet alice TxChaining.contract

      -- TODO
      -- investigate why this test failed with `valueNotConserved` error
      -- see https://github.com/Plutonomicon/cardano-transaction-lib/issues/1174
      test "Evaluation with additional UTxOs with native scripts" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 150_000_000 ]

        withWallets distribution \alice -> do
          withKeyWallet alice do
            pkh <- liftedM "Failed to get PKH" $ head <$>
              ownPaymentPubKeyHashes

            let
              constraints0 :: TxConstraints
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

              lookups0 :: Lookups.ScriptLookups
              lookups0 = mempty

            unbalancedTx0 <- mkUnbalancedTx lookups0 constraints0

            withBalancedTx unbalancedTx0 \balancedTx0 -> do
              balancedSignedTx0 <- signTransaction balancedTx0

              additionalUtxos <- createAdditionalUtxos balancedSignedTx0

              logInfo' $ "Additional utxos: " <> show additionalUtxos
              length additionalUtxos `shouldNotEqual` 0

              let
                constraints1 :: TxConstraints
                constraints1 =
                  Constraints.mustPayToPubKey pkh
                    (Value.lovelaceValueOf $ BigInt.fromInt 70_000_000)

                lookups1 :: Lookups.ScriptLookups
                lookups1 = Lookups.unspentOutputs additionalUtxos

                balanceTxConstraints
                  :: BalanceTxConstraints.BalanceTxConstraintsBuilder
                balanceTxConstraints =
                  BalanceTxConstraints.mustUseAdditionalUtxos additionalUtxos

              unbalancedTx1 <- mkUnbalancedTx lookups1 constraints1
              balancedTx1 <- balanceTxWithConstraints unbalancedTx1
                balanceTxConstraints
              balancedSignedTx1 <- signTransaction balancedTx1

              txId0 <- submit balancedSignedTx0
              txId1 <- submit balancedSignedTx1

              awaitTxConfirmed txId0
              awaitTxConfirmed txId1

      test "Evaluation with additional UTxOs" do
        -- We create two transactions. First, we create outputs with Ada, non-Ada
        -- assets, script reference with Plutus script v1 and v2, inline datum,
        -- and datum with its witness. Then, we take those outputs as additional
        -- utxos for the next transaction. After both transactions are balanced
        -- and signed, we submit them.
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 150_000_000 ]

        withWallets distribution \alice -> do
          withKeyWallet alice do
            pkh <- liftedM "Failed to get PKH" $ head <$>
              ownPaymentPubKeyHashes

            wUtxos0 <- liftedM "Failed to get wallet UTXOs" getWalletUtxos
            logInfo' $ "wUtxos0 " <> show wUtxos0

            mp <- alwaysMintsPolicyV2
            let cs = Value.scriptCurrencySymbol mp
            tn <- liftContractM "Cannot make token name"
              $ byteArrayFromAscii "TheToken" >>= Value.mkTokenName

            validatorV1 <- AlwaysSucceeds.alwaysSucceedsScript
            validatorV2 <- alwaysSucceedsScriptV2

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

              constraints0 :: TxConstraints
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

            let datumLookup = Lookups.datum datum'

            let
              lookups0 :: Lookups.ScriptLookups
              lookups0 = Lookups.mintingPolicy mp <> datumLookup

            unbalancedTx0 <- mkUnbalancedTx lookups0 constraints0

            withBalancedTx unbalancedTx0 \balancedTx0 -> do
              balancedSignedTx0 <- signTransaction balancedTx0

              additionalUtxos <- createAdditionalUtxos balancedSignedTx0

              logInfo' $ "Additional utxos: " <> show additionalUtxos
              length additionalUtxos `shouldNotEqual` 0

              let
                constraints1 :: TxConstraints
                constraints1 =
                  Constraints.mustPayToPubKey pkh $
                    Value.lovelaceValueOf (BigInt.fromInt 60_000_000)
                      <> Value.singleton cs tn (BigInt.fromInt 50)

                lookups1 :: Lookups.ScriptLookups
                lookups1 = Lookups.unspentOutputs additionalUtxos

                balanceTxConstraints
                  :: BalanceTxConstraints.BalanceTxConstraintsBuilder
                balanceTxConstraints =
                  BalanceTxConstraints.mustUseAdditionalUtxos additionalUtxos

              unbalancedTx1 <- mkUnbalancedTx lookups1 constraints1
              balancedTx1 <- balanceTxWithConstraints unbalancedTx1
                balanceTxConstraints
              balancedSignedTx1 <- signTransaction balancedTx1

              txId0 <- submit balancedSignedTx0
              txId1 <- submit balancedSignedTx1

              awaitTxConfirmed txId0
              awaitTxConfirmed txId1

    group "Application of arguments to parameterized scripts" do
      test "returns the same script when called without args" do
        withWallets unit \_ -> do
          result <- liftContractE $ applyArgs
            (unwrap unappliedScriptFixture)
            mempty
          result `shouldEqual` (unwrap unappliedScriptFixture)

      test "returns the correct partially applied Plutus script" do
        withWallets unit \_ -> do
          let args = [ Integer (BigInt.fromInt 32) ]
          result <- liftContractE $ applyArgs
            (unwrap unappliedScriptFixture)
            args
          result `shouldEqual` (unwrap partiallyAppliedScriptFixture)

      test "returns the correct fully applied Plutus script" do
        withWallets unit \_ -> do
          bytes <-
            liftContractM "Could not create ByteArray"
              (byteArrayFromAscii "test")
          let args = [ Integer (BigInt.fromInt 32), Bytes bytes ]
          result <- liftContractE $ applyArgs
            (unwrap unappliedScriptFixture)
            args
          result `shouldEqual` (unwrap fullyAppliedScriptFixture)

    group "CIP-30 mock interface" do
      test "Wallet cleanup" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          try (liftEffect $ isWalletAvailable NamiWallet) >>= flip shouldSatisfy
            isLeft
          try (liftEffect $ isWalletAvailable GeroWallet) >>= flip shouldSatisfy
            isLeft
          try (liftEffect $ isWalletAvailable FlintWallet) >>= flip
            shouldSatisfy
            isLeft
          try (liftEffect $ isWalletAvailable NuFiWallet) >>= flip shouldSatisfy
            isLeft

          withCip30Mock alice MockNami do
            (liftEffect $ isWalletAvailable NamiWallet) >>= shouldEqual true
          try (liftEffect $ isWalletAvailable NamiWallet) >>= flip shouldSatisfy
            isLeft

          withCip30Mock alice MockGero do
            (liftEffect $ isWalletAvailable GeroWallet) >>= shouldEqual true
          try (liftEffect $ isWalletAvailable GeroWallet) >>= flip shouldSatisfy
            isLeft

          withCip30Mock alice MockFlint do
            (liftEffect $ isWalletAvailable FlintWallet) >>= shouldEqual true
          try (liftEffect $ isWalletAvailable FlintWallet) >>= flip
            shouldSatisfy
            isLeft

          withCip30Mock alice MockNuFi do
            (liftEffect $ isWalletAvailable NuFiWallet) >>= shouldEqual true
          try (liftEffect $ isWalletAvailable NuFiWallet) >>= flip shouldSatisfy
            isLeft

      test "Collateral selection returns UTxO with smaller amount" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          withCip30Mock alice MockNami do
            getWalletCollateral >>= liftEffect <<< case _ of
              Nothing -> throw "Unable to get collateral"
              Just
                [ TransactionUnspentOutput
                    { output: TransactionOutputWithRefScript { output } }
                ] -> do
                let amount = (unwrap output).amount
                unless
                  (amount == lovelaceValueOf (BigInt.fromInt 50_000_000))
                  $ throw "Wrong UTxO selected as collateral"
              Just _ -> do
                throw $ "More than one UTxO in collateral. " <>
                  "Not a bug, but unexpected in this test, please update it."

      test "Get own UTxOs" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          utxos <- withCip30Mock alice MockNami do
            getWalletUtxos
          utxos `shouldSatisfy` isJust

      test "Get own address" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          mockAddress <- withCip30Mock alice MockNami do
            mbAddr <- head <$> getWalletAddresses
            mbAddr `shouldSatisfy` isJust
            pure mbAddr
          kwAddress <- head <$> withKeyWallet alice do
            getWalletAddresses
          mockAddress `shouldEqual` kwAddress

      test "Payment key hash to payment key hash Tx" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          withCip30Mock alice MockNami do
            pkh <- liftedM "Failed to get PKH" $ head <$>
              ownPaymentPubKeyHashes
            stakePkh <- join <<< head <$> ownStakePubKeyHashes
            pkh2PkhContract pkh stakePkh

      test "getWalletBalance works" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          withKeyWallet alice do
            getWalletBalance >>= shouldEqual
              ( Just $ coinToValue $ Coin $ BigInt.fromInt 1_050_000_000
              )
          withCip30Mock alice MockNami do
            getWalletBalance >>= shouldEqual
              ( Just $ coinToValue $ Coin $ BigInt.fromInt 1_050_000_000
              )

      test "getWalletBalance works (2)" do
        let
          distribution :: InitialUTxOs
          distribution =
            [ BigInt.fromInt 5_000_000
            , BigInt.fromInt 2_000_000
            , BigInt.fromInt 1_000_000
            ]
        withWallets distribution \alice -> do
          withCip30Mock alice MockNami do
            getWalletBalance >>= flip shouldSatisfy
              (eq $ Just $ coinToValue $ Coin $ BigInt.fromInt 8_000_000)

      test "CIP-30 utilities" do
        let
          distribution :: InitialUTxOsWithStakeKey
          distribution = withStakeKey privateStakeKey
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 50_000_000
            ]
        withWallets distribution \alice -> do
          withCip30Mock alice MockNami do
            Cip30.contract
          withCip30Mock alice (MockGenericCip30 "nami") do
            Cip30.contract
      test "ECDSA example" do
        let
          distribution = withStakeKey privateStakeKey
            [ BigInt.fromInt 2_000_000_000 * BigInt.fromInt 100
            , BigInt.fromInt 2_000_000_000 * BigInt.fromInt 100
            , BigInt.fromInt 2_000_000_000 * BigInt.fromInt 100
            , BigInt.fromInt 2_000_000_000 * BigInt.fromInt 100
            , BigInt.fromInt 2_000_000_000 * BigInt.fromInt 100
            ]
        withWallets distribution \alice -> do
          withCip30Mock alice MockNami $ ECDSA.contract

  group "CIP-49 Plutus Crypto Primitives" do
    test "ECDSA: a script that checks if a signature is correct" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          ECDSA.contract
    test "Schnorr: a script that checks if a signature is correct" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 50_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice do
          Schnorr.contract

signMultipleContract :: Contract Unit
signMultipleContract = do
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  stakePkh <- join <<< head <$> ownStakePubKeyHashes
  let
    constraints :: Constraints.TxConstraints
    constraints = mustPayToPubKeyStakeAddress pkh stakePkh
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  ubTx1 <- mkUnbalancedTx lookups constraints
  ubTx2 <- mkUnbalancedTx lookups constraints

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
  :: PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> Contract Unit
pkh2PkhContract pkh stakePkh = do
  let
    constraints :: Constraints.TxConstraints
    constraints = mustPayToPubKeyStakeAddress pkh stakePkh
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups
    lookups = mempty
  ubTx <- mkUnbalancedTx lookups constraints
  bsTx <- signTransaction =<< balanceTx ubTx
  submitAndLog bsTx
