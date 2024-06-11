module Test.Ctl.Plutip
  ( main
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.Address as Cardano.Plutus.Types.Address
import Cardano.Plutus.Types.OutputDatum (OutputDatum(..))
import Cardano.Serialization.Lib as CSL
import Cardano.Types
  ( Credential(PubKeyHashCredential)
  , PaymentCredential(..)
  , TransactionHash
  , TransactionInput(..)
  )
import Cardano.Types as Cardano.Types
import Cardano.Types.Address (Address, toBech32) as Cardano.Types
import Cardano.Types.Address as Cardano.Types.Address
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PrivateKey as Cardano.Type.PrivateKey
import Cardano.Types.PublicKey as Cardano.Types.PublicKey
import Contract.ClientError as Contract.ClientError
import Contract.Config (PrivatePaymentKey)
import Contract.Config as Config
import Contract.Log (logInfo)
import Contract.Monad (ContractEnv)
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
import Contract.Value
  ( Coin(..)
  , coinToValue
  , valueToCoin
  )
import Contract.Value as Contract.Value
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
import Control.Bind (bindFlipped)
import Control.Monad.Error.Class
  ( liftMaybe
  , throwError
  , try
  )
import Control.Monad.Except
  ( ExceptT(..)
  , lift
  , runExceptT
  )
import Control.Monad.Reader (local)
import Control.Parallel
  ( parallel
  , sequential
  )
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Utils
  ( EventSource
  , annotateError
  , onLine
  , tmpdir
  )
import Ctl.Internal.Testnet.Contract as Testnet.Contract
import Ctl.Internal.Testnet.Server
  ( redirectChannels
  )
import Ctl.Internal.Testnet.Server as Testnet
import Ctl.Internal.Testnet.Types
  ( CardanoTestnetStartupParams
  )
import Ctl.Internal.Testnet.Types as Testnet.Types
import Ctl.Internal.Testnet.Utils (waitFor)
import Ctl.Internal.Wallet.KeyFile (privateStakeKeyFromTextEnvelope)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.ByteArray (byteArrayToHex, hexToByteArray)
import Data.Lens (Lens', (%~))
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(..))
import Data.String as String
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error, error, message)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Internal.CardanoCli as CardanoCli
import Node.Buffer as Node.Buffer
import Node.ChildProcess as Node.ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, readTextFile)
import Node.Path (FilePath)
import Node.Stream (Readable)
import Node.Stream as Node.Stream
import Node.Stream.Aff as Node.Stream.Aff
import Record as Record
import Type.Proxy (Proxy(..))

-- | Changes TextEnvelope type to match private payment key one and tries to read that. 
readTextEnvelopeAsPaymentSkey :: FilePath -> Aff Config.PrivatePaymentKey
readTextEnvelopeAsPaymentSkey path = do
  TextEnvelope envelope <-
    liftMaybe (error "Cannot decode skey envelope")
      <<< decodeTextEnvelope
      =<< readTextFile UTF8 path
  let
    envelope' = TextEnvelope
      (envelope { type_ = PaymentSigningKeyShelleyed25519 })
  liftMaybe (error "Cannot decode payment skey from decoded envelope")
    $ privatePaymentKeyFromTextEnvelope envelope'

-- | Changes TextEnvelope type to match private staking key one and tries to read that. 
readTextEnvelopeAsStakingSkey :: FilePath -> Aff Config.PrivateStakeKey
readTextEnvelopeAsStakingSkey path = do
  TextEnvelope envelope <-
    liftMaybe (error "Cannot decode skey envelope")
      <<< decodeTextEnvelope
      =<< readTextFile UTF8 path
  let
    envelope' = TextEnvelope
      (envelope { type_ = StakeSigningKeyShelleyed25519 })
  liftMaybe (error "Cannot decode staking skey from decoded envelope")
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
-- sendFundsViaCardanoCli
--   :: { socketPath :: FilePath, testnetMagic :: Int }
--   -> { from ::
--          { txIn :: { hash :: String, id :: Int }
--          , privatePaymentKey :: PrivatePaymentKey
--          , verificationKeyHash :: String
--          , amount :: BigNum
--          }
--      , changeAddress :: String
--      , to :: { address :: String }
--      }
--   -> Aff TransactionHash
-- sendFundsViaCardanoCli { socketPath, testnetMagic } { from, changeAddress, to } =
--   do
--     -- every instance have it's own dir for temp files
--     workdir <- liftEffect do
--       tmp <- tmpdir
--       operationId <- show <$> randomInt 0 1000
--       pure $ tmp <</>> "transaction-" <> operationId
--     let
--       keyFile = workdir <</>> "signature.skey"
--       txBodyFile = workdir <</>> "tx-body.json"
--       txFile = workdir <</>> "tx.json"
--     mkdir workdir
--     privatePaymentKeyToFile keyFile from.privatePaymentKey
--     do
--       { stdout } <- execCardanoCli
--         [ "transaction"
--         , "build"
--         , "--socket-path"
--         , socketPath
--         , "--testnet-magic"
--         , show testnetMagic
--         , "--tx-in"
--         , from.txIn.hash <> "#" <> show from.txIn.id
--         , "--tx-out"
--         , to.address <> "+" <> BigNum.toString from.amount
--         , "--required-signer-hash"
--         , from.verificationKeyHash
--         , "--required-signer"
--         , keyFile
--         , "--change-address"
--         , changeAddress
--         , "--out-file"
--         , txBodyFile
--         ]
--       for_ stdout $ log <<< append "cardano-cli/build-transaction"
--     do
--       { stdout } <- execCardanoCli
--         [ "transaction"
--         , "sign"
--         , "--tx-body-file"
--         , txBodyFile
--         , "--signing-key-file"
--         , keyFile
--         , "--testnet-magic"
--         , show testnetMagic
--         , "--out-file"
--         , txFile
--         ]
--       for_ stdout $ log <<< append "cardano-cli/sign-transaction"
--     txHash <- do
--       { stdout } <- execCardanoCli
--         [ "transaction"
--         , "txid"
--         , "--tx-file"
--         , txFile
--         ]
--       txHash <- case stdout of
--         [txHash] -> pure txHash
--         _ -> throwError $ error $ "cardano-cli/tx-hash: wrong output lines: " <> show stdout
--       liftMaybe (error "Cannot parse tx hash")
--         $ map wrap
--         <<< CSL.fromBytes
--         =<< hexToByteArray txHash
--     do
--       { stdout } <- execCardanoCli
--         [ "transaction"
--         , "submit"
--         , "--socket-path"
--         , socketPath
--         , "--testnet-magic"
--         , show testnetMagic
--         , "--tx-file"
--         , txFile
--         ]
--       for_ stdout $ log <<< append "cardano-cli/submit-transaction"
--     pure txHash

--unsafeThrow "asdfsadf"

type UtxosAtQuery =
  Cardano.Types.Address
  -> Aff (Either Contract.ClientError.ClientError Cardano.Types.UtxoMap)

utxosAtWithCardanoCli
  :: CardanoCli.CardanoNodeInstance
  -> UtxosAtQuery
  -> UtxosAtQuery
utxosAtWithCardanoCli node utxosAt address = runExceptT do
  let
    toCliError :: Error -> Contract.ClientError.ClientError
    toCliError = Contract.ClientError.ClientOtherError <<< message

    toUtxoMap :: Array CardanoCli.CardanoCliTxOutInfo -> Cardano.Types.UtxoMap
    toUtxoMap = Map.fromFoldable
      <<< map (CardanoCli.cardanoCliTxOutInfoToUtxo address)
  cardanoCliUtxos <- ExceptT
    $ map (bimap toCliError toUtxoMap)
    $ try
    $ CardanoCli.queryUtxosViaCardanoCli node address
  kupoUtxos <- ExceptT $ utxosAt address
  pure $ Map.union kupoUtxos cardanoCliUtxos

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
        -- getAddressPaymentPkh addr = liftMaybe (error "Cannot get own pkh")
        --   $ Cardano.Types.Address.getPaymentCredential addr
        --   >>= case _ of
        --     PaymentCredential (PubKeyHashCredential pkh) -> Just pkh
        --     _ -> Nothing

        -- | Creates wallet from a payment key in TextEnvelope of any .type as if it was private payment key type.  
        readHackWallet
          :: forall m. MonadAff m => String -> Int -> m (String /\ KeyWallet)
        readHackWallet name idx = liftAff do
          let identifier = name <> show idx
          wallet <- walletFromHackedKeys (identifier <> ".skey") Nothing
          pure $ identifier /\ wallet

        walletFromHackedKeys payment staking = do
          pk <- readTextEnvelopeAsPaymentSkey payment
          sk <- traverse readTextEnvelopeAsStakingSkey staking
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
          -- genesisUtxos <- for addresses \addr -> do
          --   ownPkh <- getAddressPaymentPkh addr
          --   -- cardano-cli will show some funds
          --   utxo@{ txId, txOutId } <- liftAff $ queryUtxosViaCardanoCli nodeCfg addr >>= case _ of
          --     [utxo] -> pure utxo
          --     _ -> throwError $ error "Expected exactly one genesis UTxO"
          --   pure $ addr /\ utxo
          -- -- sending to the new wallet defined above
          -- txHash <- liftAff $ sendFundsViaCardanoCli
          --   nodeCfg
          --   { from:
          --       { txIn:
          --           { hash: byteArrayToHex $ CSL.toBytes $ unwrap txId
          --           , id: UInt.toInt txOutId
          --           }
          --       , amount: BigNum.fromInt 300000000
          --       , privatePaymentKey: w.paymentKey
          --       , verificationKeyHash:
          --           byteArrayToHex $ CSL.toBytes $ unwrap ownPkh
          --       }
          --   , changeAddress: Cardano.Types.Address.toBech32 addr
          --   , to:
          --       { address: Cardano.Types.Address.toBech32 newWallet.address
          --       }
          --   }
          -- awaitTxConfirmed txHash
          -- log "Confirmed"
          -- Kupo does see funds on the new wallet
          local (utxosAtL %~ utxosAtWithCardanoCli nodeCfg) do
            log <<< show <<< { utxosAfterTx: _ } <<< map valueToCoin =<<
              getWalletBalance -- utxosAt newWallet.address

  log <<< append "Tmp dir is: " =<< liftEffect tmpdir

-- | txos
utxosAtL :: Lens' ContractEnv UtxosAtQuery
utxosAtL = prop (Proxy :: _ "handle") <<< prop (Proxy :: _ "utxosAt")

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

