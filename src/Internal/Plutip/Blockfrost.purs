module Ctl.Internal.Plutip.Blockfrost
  ( runPlutipTestsWithKeyDir
  ) where

import Prelude

import Contract.Address (getWalletAddresses, ownPaymentPubKeysHashes)
import Contract.Config (ContractParams)
import Contract.Hashing (publicKeyHash)
import Contract.Log (logError', logTrace')
import Contract.Monad
  ( Contract
  , ContractEnv
  , liftedE
  , liftedM
  , runContractInEnv
  , withContractEnv
  )
import Contract.TextEnvelope (decodeTextEnvelope)
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  , submitTxFromConstraints
  )
import Contract.Utxos (utxosAt)
import Contract.Value (valueToCoin')
import Contract.Wallet (privateKeysToKeyWallet, withKeyWallet)
import Contract.Wallet.Key
  ( keyWalletPrivatePaymentKey
  , keyWalletPrivateStakeKey
  , publicKeyFromPrivateKey
  )
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromTextEnvelope
  , privatePaymentKeyToFile
  , privateStakeKeyFromTextEnvelope
  , privateStakeKeyToFile
  )
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, local)
import Ctl.Internal.Deserialization.Keys (freshPrivateKey)
import Ctl.Internal.Helpers (logWithLevel)
import Ctl.Internal.Plutip.Server (PlutipTest(PlutipTest))
import Ctl.Internal.Plutip.Types
  ( PrivateKeyResponse(PrivateKeyResponse)
  , UtxoAmount
  )
import Ctl.Internal.Plutip.UtxoDistribution
  ( decodeWallets
  , encodeDistribution
  , keyWallets
  )
import Ctl.Internal.Plutus.Types.Transaction (_amount, _output)
import Ctl.Internal.Plutus.Types.Value (Value, lovelaceValueOf)
import Ctl.Internal.Serialization.Address (addressBech32)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.ScriptLookups (mkUnbalancedTx, unspentOutputs)
import Ctl.Internal.Types.TxConstraints
  ( TxConstraint(MustPayToPubKeyAddress)
  , TxConstraints
  , mustBeSignedBy
  , mustPayToPubKeyAddress
  , mustSpendPubKeyOutput
  , singleton
  )
import Ctl.Internal.Wallet (KeyWallet)
import Data.Array (catMaybes)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left), hush)
import Data.Foldable (fold)
import Data.Lens ((^.))
import Data.List (List(Cons, Nil))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), isNothing, maybe)
import Data.Newtype (over, unwrap, wrap)
import Data.String (joinWith)
import Data.String.Utils (startsWith) as String
import Data.Traversable (foldMap, for, for_, traverse, traverse_)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, Milliseconds(Milliseconds), try)
import Effect.Aff (bracket) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Aff.Retry
  ( RetryPolicy
  , constantDelay
  , limitRetriesByCumulativeDelay
  , recovering
  )
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console as Console
import Effect.Exception (error)
import Effect.Ref as Ref
import Mote.Monad (mapTest)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (exists, mkdir, readTextFile, readdir, writeTextFile)
import Node.Path (FilePath)
import Node.Path (concat) as Path
import Type.Prelude (Proxy(Proxy))

-- | Run `PlutipTest`s given `ContractParams` value, not necessarily containing
-- | references to runtime services started with Plutip.
-- | This function can be used to interpret `TestPlanM PlutipTest` in any
-- | environment.
-- |
-- | Tests are funded by the wallet in the supplied environment.
-- | The `FilePath` parameter should point to a directory to store generated
-- | wallets, in the case where funds failed to be returned to the main wallet.
runPlutipTestsWithKeyDir
  :: ContractParams
  -> FilePath
  -> TestPlanM PlutipTest Unit
  -> TestPlanM (Aff Unit) Unit
runPlutipTestsWithKeyDir params backup = do
  mapTest \(PlutipTest runPlutipTest) -> do
    withContractEnv params \env -> do
      keyWallets <- liftAff $ restoreWallets backup
      when (Array.length keyWallets > 0) do
        liftEffect $ Console.log
          "Checking the backup wallets for leftover funds..."
        returnFunds backup env keyWallets Nothing
    runPlutipTest \distr mkTest -> withContractEnv params \env -> do
      let
        distrArray :: Array (Array UtxoAmount)
        distrArray = encodeDistribution distr

      privateKeys <- liftEffect $ for distrArray \_ -> freshPrivateKey <#>
        PrivateKeyResponse

      wallets <-
        liftMaybe
          ( error
              "Impossible happened: could not decode wallets. Please report as bug"
          )
          $ decodeWallets distr privateKeys

      let
        walletsArray :: Array KeyWallet
        walletsArray = keyWallets (pureProxy distr) wallets

        runContract :: Aff Unit
        runContract = runContractInEnv env { wallet = Nothing } do
          mkTest wallets

      if Array.null walletsArray then
        runContract
      else Aff.bracket
        ( backupWallets backup env walletsArray *> fundWallets env walletsArray
            distrArray
        )
        -- Retry fund returning until success or timeout. Submission will fail if
        -- the node has seen the wallets utxos being spent previously, so retrying
        -- will allow the wallets utxos to eventually represent a spendable set
        ( \funds -> recovering returnFundsRetryPolicy ([ \_ _ -> pure true ])
            \_ -> returnFunds backup env walletsArray (Just funds) *>
              runContractInEnv env (markAsInactive backup walletsArray)
        )
        \_ -> runContract
  where
  pureProxy :: forall (a :: Type). a -> Proxy a
  pureProxy _ = Proxy

-- | Marking wallet as inactive essentially says that we think that there are no
-- | funds left there, but we don't want to delete the files because there may
-- | be some blockchain state that is tied to the keys, e.g. funds locked on
-- | scripts, so we preserve the private keys for the rare occasions when the
-- | user may want to use them later.
-- |
-- | Marking wallet as inactive allows the test suite to skip it during fund
-- | recovery attempts that happen before each test run.
markAsInactive :: FilePath -> Array KeyWallet -> Contract Unit
markAsInactive backup wallets = do
  for_ wallets \wallet -> do
    networkId <- asks _.networkId
    let
      address = addressBech32 $ (unwrap wallet).address networkId
      inactiveFlagFile = Path.concat [ backup, address, "inactive" ]
    liftAff $ writeTextFile UTF8 inactiveFlagFile $
      "This address was marked as inactive. "
        <> "The test suite assumes that there are no funds left on it. "
        <> "You can check it by inspecting the address on a chain explorer."

-- | Restore all wallets from the backups that are not marked as inactive.
-- | Normally, this function returns the wallets that have some funds of them
-- | left, e.g. those who were not emptied because the user hit Ctrl+C
restoreWallets :: FilePath -> Aff (Array KeyWallet)
restoreWallets backup = do
  walletDirs <- readdir backup <#> Array.filter (String.startsWith "addr")
  catMaybes <$> for walletDirs \walletDir -> do
    let
      paymentKeyFilePath = Path.concat
        [ backup, walletDir, "payment_signing_key" ]
      stakeKeyFilePath = Path.concat
        [ backup, walletDir, "stake_signing_key" ]
      inactiveFlagFile = Path.concat [ backup, walletDir, "inactive" ]
    -- Skip this wallet if it was marked as inactive
    exists inactiveFlagFile >>= case _ of
      true -> pure Nothing
      false -> do
        paymentKeyEnvelope <- readTextFile UTF8 paymentKeyFilePath
        paymentKey <-
          liftMaybe
            ( error $ "Unable to decode private payment key from " <>
                paymentKeyFilePath
            ) $
            privatePaymentKeyFromTextEnvelope =<< decodeTextEnvelope
              paymentKeyEnvelope
        mbStakeKey <- do
          mbStakeKeyEnvelope <- hush <$> try do
            readTextFile UTF8 stakeKeyFilePath
          for mbStakeKeyEnvelope \stakeKeyEnvelope -> do
            liftMaybe
              ( error $ "Unable to decode private stake key from " <>
                  stakeKeyFilePath
              ) $
              privateStakeKeyFromTextEnvelope =<< decodeTextEnvelope
                stakeKeyEnvelope
        pure $ Just $ privateKeysToKeyWallet paymentKey mbStakeKey

-- | Save wallets to files in the backup directory for private keys
backupWallets :: FilePath -> ContractEnv -> Array KeyWallet -> Aff Unit
backupWallets backup env walletsArray = liftAff $ for_ walletsArray \wallet ->
  do
    let
      address = addressBech32 $ (unwrap wallet).address env.networkId
      payment = keyWalletPrivatePaymentKey wallet
      mbStake = keyWalletPrivateStakeKey wallet
      folder = Path.concat [ backup, address ]

    mkdir folder
    privatePaymentKeyToFile (Path.concat [ folder, "payment_signing_key" ])
      payment
    for mbStake $ privateStakeKeyToFile
      (Path.concat [ folder, "stake_signing_key" ])

-- | Create a transaction that builds a specific UTxO distribution on the wallets.
fundWallets
  :: ContractEnv -> Array KeyWallet -> Array (Array UtxoAmount) -> Aff BigInt
fundWallets env walletsArray distrArray = runContractInEnv env $ noLogs do
  logTrace' "Funding wallets"
  let
    constraints = flip foldMap (Array.zip walletsArray distrArray)
      \(wallet /\ walletDistr) -> flip foldMap walletDistr
        \value -> mustPayToKeyWallet wallet $ lovelaceValueOf value

  txHash <- submitTxFromConstraints (mempty :: _ Void) constraints
  awaitTxConfirmed txHash
  let fundTotal = Array.foldl (+) zero $ join distrArray
  -- Use log so we can see, regardless of suppression
  log $ joinWith " "
    [ "Sent"
    , BigInt.toString fundTotal
    , "lovelace to test wallets"
    ]
  pure fundTotal

-- Suppress logs during wallet funding, and throw an informative exception
noLogs :: forall (a :: Type). Contract a -> Contract a
noLogs action = do
  logRef <- liftEffect $ Ref.new Nil
  let
    voidLogger env = env
      { customLogger = Just
          \level message ->
            liftEffect $ Ref.modify_ (Cons $ level /\ message) logRef
      }
  eiRes <- try $ local voidLogger action
  case eiRes of
    Left err -> do
      logError' "-------- BEGIN LOGS FOR WALLET FUNDS REDISTRIBUTION --------"
      logError' $ "The lines below are not coming from the Contract that is "
        <> "under test"
      liftEffect (Ref.read logRef <#> List.reverse) >>= traverse_
        \(level /\ message) -> do
          liftEffect $ logWithLevel level message
      logError' "--------- END LOGS FOR WALLET FUNDS REDISTRIBUTION ---------"
      throwError $ error $
        "An exception has been thrown during funds redistribution in \
        \Blockfrost test suite. Most likely, the wallet ran out of funds. \
        \It is probably not a problem with the Contract that is being tested. \
        \The error was: " <> show err
    Right res -> pure res

returnFundsRetryPolicy :: RetryPolicy
returnFundsRetryPolicy = limitRetriesByCumulativeDelay
  (Milliseconds 30_000.00)
  (constantDelay $ Milliseconds 2_000.0)

-- | Find all non-empty wallets and return the funds.
-- | Accepts an optional expected Lovelace number to be returned.
returnFunds
  :: FilePath -> ContractEnv -> Array KeyWallet -> Maybe BigInt -> Aff Unit
returnFunds backup env allWalletsArray mbFundTotal = runContractInEnv env $
  noLogs do
    nonEmptyWallets <- catMaybes <$> for allWalletsArray \wallet -> do
      withKeyWallet wallet do
        utxoMap <- liftedM "Failed to get utxos" $
          (getWalletAddresses <#> Array.head) >>= traverse utxosAt
        if Map.isEmpty utxoMap then do
          markAsInactive backup [ wallet ]
          pure Nothing
        else pure $ Just (utxoMap /\ wallet)

    when (Array.length nonEmptyWallets /= 0) do
      -- Print the messages only if we are running during initial fund recovery
      when (isNothing mbFundTotal) do
        log $ "Non-empty wallets found: " <> show (Array.length nonEmptyWallets)
        log $ "Trying to return the funds back to the main wallet before " <>
          "starting the test suite..."
      let utxos = nonEmptyWallets # map fst # Map.unions

      pkhs <- fold <$> for nonEmptyWallets
        (snd >>> flip withKeyWallet ownPaymentPubKeysHashes)

      let
        constraints = flip foldMap (Map.keys utxos) mustSpendPubKeyOutput
          <> foldMap mustBeSignedBy pkhs
        lookups = unspentOutputs utxos

      unbalancedTx <- liftedE $ mkUnbalancedTx (lookups :: _ Void) constraints
      balancedTx <- liftedE $ balanceTx unbalancedTx
      balancedSignedTx <- Array.foldM
        (\tx wallet -> withKeyWallet wallet $ signTransaction tx)
        (wrap $ unwrap balancedTx)
        (nonEmptyWallets <#> snd)

      txHash <- submit balancedSignedTx
      awaitTxConfirmed txHash

      let
        (refundTotal :: BigInt) = Array.foldl
          (\acc txorf -> acc + valueToCoin' (txorf ^. _output ^. _amount))
          zero
          (Array.fromFoldable $ Map.values utxos)

      log $ joinWith " " $
        [ "Refunded"
        , BigInt.toString refundTotal
        , "Lovelace"
        ] <> maybe []
          ( \fundTotal ->
              [ "of"
              , BigInt.toString fundTotal
              , "Lovelace from test wallets"
              ]
          )
          mbFundTotal

-- | A helper function that abstracts away conversion between `KeyWallet` and
-- | its address and just gives us a `TxConstraints` value.
mustPayToKeyWallet
  :: forall (i :: Type) (o :: Type)
   . KeyWallet
  -> Value
  -> TxConstraints i o
mustPayToKeyWallet wallet value =
  let
    convert = wrap <<< publicKeyHash <<< publicKeyFromPrivateKey
    payment = over wrap convert $ keyWalletPrivatePaymentKey wallet
    mbStake = over wrap convert <$> keyWalletPrivateStakeKey wallet
  in
    maybe
      -- We don't use `mustPayToPubKey payment` to avoid the compile-time
      -- warning that is tied to it (it should not be propagated to
      -- `runPlutipTestsWithKeyDir`)
      (singleton <<< MustPayToPubKeyAddress payment Nothing Nothing Nothing)
      (mustPayToPubKeyAddress payment)
      mbStake
      value
