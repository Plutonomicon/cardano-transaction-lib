module Test.Ctl.Plutip
  ( main
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.Address as Cardano.Plutus.Types.Address
import Cardano.Serialization.Lib as CSL
import Cardano.Types
  ( Credential(PubKeyHashCredential)
  , PaymentCredential(..)
  , TransactionHash
  )
import Cardano.Types (NetworkId(TestnetId)) as Cardano.Types
import Cardano.Types.Address (Address, toBech32) as Cardano.Types
import Cardano.Types.Address as Cardano.Types.Address
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PrivateKey as Cardano.Type.PrivateKey
import Cardano.Types.PublicKey as Cardano.Types.PublicKey
import Contract.Config (PrivatePaymentKey)
import Contract.Config as Config
import Contract.Monad (runContractInEnv) as Contract
import Contract.Test.Plutip (defaultPlutipConfig)
import Contract.Test.Utils
  ( interruptOnSignal
  )
import Contract.TextEnvelope
  ( TextEnvelope(..)
  , TextEnvelopeType(..)
  , decodeTextEnvelope
  )
import Contract.Transaction (awaitTxConfirmed)
import Contract.Utxos (utxosAt)
import Contract.Wallet
  ( getWalletAddresses
  , getWalletBalance
  , mkKeyWalletFromPrivateKeys
  , withKeyWallet
  )
import Contract.Wallet.Key (KeyWallet(..))
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromTextEnvelope
  , privatePaymentKeyToFile
  )
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Utils (EventSource, onLine, tmpdir)
import Ctl.Internal.Testnet.Contract as Testnet.Contract
import Ctl.Internal.Testnet.Server
  ( redirectChannels
  )
import Ctl.Internal.Testnet.Server as Testnet
import Ctl.Internal.Testnet.Types
  ( CardanoTestnetStartupParams
  )
import Ctl.Internal.Testnet.Types as Testnet.Types
import Ctl.Internal.Testnet.Utils
  ( waitFor
  )
import Ctl.Internal.Wallet.KeyFile (privateStakeKeyFromTextEnvelope)
import Data.Array as Array
import Data.ByteArray (byteArrayToHex, hexToByteArray)
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(..))
import Data.String as String
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error, error)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Node.Buffer as Node.Buffer
import Node.ChildProcess as Node.ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, readTextFile)
import Node.Path (FilePath)
import Node.Stream as Node.Stream
import Record as Record

foreign import readableFromBuffer
  :: Node.Buffer.Buffer -> Effect (Node.Stream.Readable ())

readGenesisPkey :: FilePath -> Aff Config.PrivatePaymentKey
readGenesisPkey path = do
  TextEnvelope envelope <-
    liftMaybe (error "Cannot decode genesis pkey envelope")
      <<< decodeTextEnvelope
      =<< readTextFile UTF8 path
  let
    envelope' = TextEnvelope
      (envelope { type_ = PaymentSigningKeyShelleyed25519 })
  liftMaybe (error "Cannot decode genesis pkey from decoded envelope")
    $ privatePaymentKeyFromTextEnvelope envelope'

readGenesisStakingPkey :: FilePath -> Aff Config.PrivateStakeKey
readGenesisStakingPkey path = do
  TextEnvelope envelope <-
    liftMaybe (error "Cannot decode genesis pkey envelope")
      <<< decodeTextEnvelope
      =<< readTextFile UTF8 path
  let
    envelope' = TextEnvelope
      (envelope { type_ = StakeSigningKeyShelleyed25519 })
  liftMaybe (error "Cannot decode genesis pkey from decoded envelope")
    $ privateStakeKeyFromTextEnvelope envelope'

allToConsolePrefixed
  :: String
  -> { stdout :: EventSource String
     , stderr :: EventSource String
     }
  -> Aff Unit
allToConsolePrefixed prefix channels = void $ redirectChannels channels
  { stderrTo:
      { console: Just $ "[" <> prefix <> "][stderr]"
      , log: Nothing
      }
  , stdoutTo:
      { console: Just $ "[" <> prefix <> "][stdout]"
      , log: Nothing
      }
  }

-- | Send lovelace from one address to another, requiring only one signature
sendFundsViaCardanoCli
  :: { socketPath :: FilePath, testnetMagic :: Int }
  -> { from ::
         { txIn :: { hash :: String, id :: Int }
         , privatePaymentKey :: PrivatePaymentKey
         , verificationKeyHash :: String
         , amount :: BigNum
         }
     , changeAddress :: String
     , to :: { address :: String }
     }
  -> Aff TransactionHash
sendFundsViaCardanoCli { socketPath, testnetMagic } { from, changeAddress, to } =
  do
    -- every instance have it's own dir for temp files
    workdir <- liftEffect do
      tmp <- tmpdir
      operationId <- show <$> randomInt 0 1000
      pure $ tmp <</>> "transaction-" <> operationId
    let
      keyFile = workdir <</>> "signature.skey"
      txBodyFile = workdir <</>> "tx-body.json"
      txFile = workdir <</>> "tx.json"
    mkdir workdir
    privatePaymentKeyToFile keyFile from.privatePaymentKey
    { channels: buildTxChannel } <- execCardanoCli
      [ "transaction"
      , "build"
      , "--socket-path"
      , socketPath
      , "--testnet-magic"
      , show testnetMagic
      , "--tx-in"
      , from.txIn.hash <> "#" <> show from.txIn.id
      , "--tx-out"
      , to.address <> "+" <> BigNum.toString from.amount
      , "--required-signer-hash"
      , from.verificationKeyHash
      , "--required-signer"
      , keyFile
      , "--change-address"
      , changeAddress
      , "--out-file"
      , txBodyFile
      ]
    allToConsolePrefixed "cardano-cli/build-transaction" buildTxChannel
    { channels: signTxChannel } <- execCardanoCli
      [ "transaction"
      , "sign"
      , "--tx-body-file"
      , txBodyFile
      , "--signing-key-file"
      , keyFile
      , "--testnet-magic"
      , show testnetMagic
      , "--out-file"
      , txFile
      ]
    allToConsolePrefixed "cardano-cli/sign-transaction" signTxChannel
    { channels: txIdChannel } <- execCardanoCli
      [ "transaction"
      , "txid"
      , "--tx-file"
      , txFile
      ]
    allToConsolePrefixed "cardano-cli/transaction-id" txIdChannel
    txHash <- waitFor txIdChannel.stdout Just -- the only output is a tx hash
    { channels: submitTxChannel } <- execCardanoCli
      [ "transaction"
      , "submit"
      , "--socket-path"
      , socketPath
      , "--testnet-magic"
      , show testnetMagic
      , "--tx-file"
      , txFile
      ]
    allToConsolePrefixed "cardano-cli/submit-transaction" submitTxChannel
    liftMaybe (error "Cannot parse tx hash")
      $ map wrap
      <<< CSL.fromBytes
      =<< hexToByteArray txHash

-- | Queries address funds via cardano-cli and returns the first UTxO data.
queryUtxosViaCardanoCli
  :: { socketPath :: FilePath
     , testnetMagic :: Int
     }
  -> Cardano.Types.Address
  -> Aff CardanoCliTxOutInfo
queryUtxosViaCardanoCli { socketPath, testnetMagic } address = do
  { channels } <- execCardanoCli
    [ "query"
    , "utxo"
    , "--socket-path"
    , socketPath
    , "--testnet-magic"
    , show testnetMagic
    , "--address"
    , Cardano.Types.toBech32 address
    ]
  allToConsolePrefixed "cardano-cli/query-utxo" channels
  waitFor channels.stdout parseTxOut

type CardanoCliTxOutInfo =
  { txId :: TransactionHash, txOutId :: UInt, amount :: BigNum }

parseTxOut :: String -> Maybe CardanoCliTxOutInfo
parseTxOut src = do
  let
    words = String.split (String.Pattern " ") src
    nonEmptyWords = Array.take 3 $ Array.filter (not <<< String.null) words
  { hash, id, amt } <- case nonEmptyWords of
    [ hash, id, amt ] -> Just { hash, id, amt }
    _ -> Nothing
  txId <- map wrap <<< CSL.fromBytes =<< hexToByteArray hash
  txOutId <- UInt.fromString id
  amount <- BigNum.fromString amt
  pure { txId, txOutId, amount }

defaultStartupParams :: { | CardanoTestnetStartupParams () }
defaultStartupParams =
  ( Testnet.Types.defaultStartupParams
      { testnetMagic: 2 }
  )
    { nodeLoggingFormat = Just Testnet.Types.LogAsJson }

-- Run with `npm run plutip-test`
main :: Effect Unit
main = (interruptOnSignal SIGINT =<< _) $ launchAff $ void do
  let cfg = Record.union defaultStartupParams defaultPlutipConfig
  _ <- Testnet.Contract.withContractEnv cfg \cluster env -> do
    log "Running test contract"
    Contract.runContractInEnv env do
      log "Inside"
      let
        Testnet.MkStartedTestnetCluster { paths } = cluster
        v872utxoKeysDir = paths.testnetDirectory <</>> "utxo-keys"

      -- a new wallet to send initial lovelace to
      newWallet <- do
        newPk <- wrap <<< wrap <$> liftEffect CSL.privateKey_generateEd25519
        let
          pkh = Cardano.Types.PublicKey.hash
            $ Cardano.Type.PrivateKey.toPublicKey
            $ unwrap newPk
          wallet = mkKeyWalletFromPrivateKeys newPk Nothing
        address <-
          liftMaybe (error "Cannot convert Plutus address to Cardano address")
            $ Cardano.Plutus.Types.Address.toCardano Cardano.Types.TestnetId
            $ Cardano.Plutus.Types.Address.pubKeyHashAddress
                (wrap $ wrap pkh)
                Nothing
        pure { pkh, wallet, address }

      let
        getAddressPaymentPkh addr = liftMaybe (error "Cannot get own pkh")
          $ Cardano.Types.Address.getPaymentCredential addr
          >>= case _ of
            PaymentCredential (PubKeyHashCredential pkh) -> Just pkh
            _ -> Nothing

        -- | Creates wallet from a payment key in TextEnvelope of any .type as if it was private payment key type.  
        readHackWallet
          :: forall m. MonadAff m => String -> Int -> m (String /\ KeyWallet)
        readHackWallet name idx = liftAff do
          let identifier = name <> show idx
          wallet <- walletFromHackedKeys (identifier <> ".skey") Nothing
          pure $ identifier /\ wallet

        walletFromHackedKeys payment staking = do
          pk <- readGenesisPkey payment
          sk <- traverse readGenesisStakingPkey staking
          pure $ mkKeyWalletFromPrivateKeys pk sk

      -- do something interesting with every utxo in utxo-keys
      for_ [ 1, 2, 3 ] \idx -> do
        walletName /\ wallet@(KeyWallet w) <-
          readHackWallet (v872utxoKeysDir <</>> "utxo") idx
        withKeyWallet wallet do
          let
            nodeCfg =
              { socketPath: paths.nodeSocketPath
              , testnetMagic: cfg.testnetMagic
              }
          addresses <- getWalletAddresses
          -- it will show zero balance
          log
            <<< show
            <<< { wallet: walletName, initialBalanceViaKupo: _ }
            =<< getWalletBalance
          -- so let's try to use cardano-cli to resend funds and then Kupo will see it
          for_ addresses \addr -> do
            ownPkh <- getAddressPaymentPkh addr
            -- cardano-cli will show some funds
            { txId, txOutId } <- liftAff $ queryUtxosViaCardanoCli nodeCfg addr
            -- sending to the new wallet defined above
            txHash <- liftAff $ sendFundsViaCardanoCli
              nodeCfg
              { from:
                  { txIn:
                      { hash: byteArrayToHex $ CSL.toBytes $ unwrap txId
                      , id: UInt.toInt txOutId
                      }
                  , amount: BigNum.fromInt 300000000
                  , privatePaymentKey: w.paymentKey
                  , verificationKeyHash:
                      byteArrayToHex $ CSL.toBytes $ unwrap ownPkh
                  }
              , changeAddress: Cardano.Types.Address.toBech32 addr
              , to:
                  { address: Cardano.Types.Address.toBech32 newWallet.address
                  }
              }
            awaitTxConfirmed txHash
            log "Confirmed"
            -- Kupo does see funds on the new wallet
            log <<< show <<< { utxosAfterTx: _ } =<< utxosAt newWallet.address

  log <<< append "Tmp dir is: " =<< liftEffect tmpdir

-- flip cancelWith (effectCanceler (exitCode 1)) do
--   Utils.interpretWithConfig
--     defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true }
--     $ group "Plutip" do
--         testTestnetContracts config Mnemonics.suite
--         group "ExUnits - normal limits" do
--           testTestnetContracts config $ ExUnits.mkFailingSuite 3000
--           testTestnetContracts config $ ExUnits.mkSuite 2550
--         group "ExUnits - relaxed limits" do
--           testTestnetContracts configWithMaxExUnits $ ExUnits.mkSuite 3000
--         testTestnetContracts config Assert.suite
--         Logging.suite
--         testStartTestnet
--         testTestnetContracts config $ do
--           flip mapTest QueryM.AffInterface.suite
--             (noWallet <<< wrapQueryM)
--           ChangeGeneration.suite
--           Contract.suite
--         UtxoDistribution.suite
--         testTestnetContracts config OgmiosMempool.suite
--         runTestnetTestPlan config SameWallets.suite
--         ClusterParameters.runTest

-- configWithMaxExUnits :: PlutipConfig
-- configWithMaxExUnits = config
--   { clusterConfig = config.clusterConfig { raiseExUnitsToMax = true } }

-- testStartTestnet:: TestPlanM (Aff Unit) Unit
-- testStartTestnet = group "Server" do
--   test "startTestnet / stopTestnet" do
--     -- bracket (startTestnet config)
--     --   (stopChildProcessWithPort config.port) $ const do
--       checkTestnet config
--       _startRes <- startTestnet config [ [] ]
--       stopRes <- stopTestnet config
--       stopRes `shouldSatisfy` case _ of
--         StopClusterSuccess -> true
--         _ -> false

execCardanoCli
  :: Array String
  -> Aff
       { channels ::
           { stdout :: EventSource String
           , stderr :: EventSource String
           }
       , process :: Node.ChildProcess.ChildProcess
       }
execCardanoCli params = Aff.makeAff \cont -> do
  processRef <- Ref.new Nothing
  let cmd = "cardano-cli " <> intercalate " " params
  log $ show { execCardanoCli: cmd }
  process <- Node.ChildProcess.exec
    cmd
    Node.ChildProcess.defaultExecOptions
    ( \{ error: err, stderr, stdout } -> do
        process <-
          liftMaybe (error "Couldn't find executed process" :: Error)
            =<< Ref.read processRef
        stderrStream <- readableFromBuffer stderr :: Effect _
        stdoutStream <- readableFromBuffer stdout
        channels <- liftEffect
          $ { stderr: _, stdout: _ }
          <$> onLine stderrStream Just
          <*> onLine stdoutStream Just
        let
          result =
            { channels
            , process
            }
        cont $ maybe (Right result) Left err
    )
  Ref.write (Just process) processRef
  pure $ Aff.Canceler \err -> liftEffect $ cont $ Left err