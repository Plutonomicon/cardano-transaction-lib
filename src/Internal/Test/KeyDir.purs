module Ctl.Internal.Test.KeyDir
  ( runContractTestsWithKeyDir
  ) where

import Prelude

import Cardano.Types (BigNum, Value, _amount)
import Cardano.Types.Address (toBech32) as Address
import Cardano.Types.BigNum as BigNum
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.PrivateKey as PrivateKey
import Cardano.Types.PublicKey as PublicKey
import Cardano.Types.Value as Value
import Cardano.Wallet.Key (KeyWallet)
import Contract.Config (ContractParams)
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
import Contract.Wallet
  ( getWalletAddresses
  , getWalletBalance
  , ownPaymentPubKeyHashes
  , privateKeysToKeyWallet
  , withKeyWallet
  )
import Contract.Wallet.Key
  ( getPrivatePaymentKey
  , getPrivateStakeKey
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
import Control.Parallel (parTraverse, parTraverse_)
import Ctl.Internal.Helpers (logWithLevel, unsafeFromJust)
import Ctl.Internal.ProcessConstraints (mkUnbalancedTxImpl)
import Ctl.Internal.Test.ContractTest (ContractTest(ContractTest))
import Ctl.Internal.Test.UtxoDistribution
  ( UtxoAmount
  , decodeWallets
  , encodeDistribution
  , keyWallets
  )
import Ctl.Internal.Types.ScriptLookups (unspentOutputs)
import Ctl.Internal.Types.TxConstraints
  ( TxConstraint(MustPayToPubKeyAddress)
  , TxConstraints
  , mustBeSignedBy
  , mustPayToPubKeyAddress
  , mustSpendPubKeyOutput
  )
import Data.Array (catMaybes, singleton)
import Data.Array as Array
import Data.Either (Either(Right, Left), hush)
import Data.Foldable (fold, sum)
import Data.Lens ((^.))
import Data.List (List(Cons, Nil))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), isNothing, maybe)
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
import Effect.Class.Console (info)
import Effect.Exception (error, throw)
import Effect.Ref as Ref
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Mote.Monad (mapTest)
import Mote.TestPlanM (TestPlanM)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (mkdir, readTextFile, readdir, writeTextFile)
import Node.FS.Sync (exists)
import Node.Path (FilePath)
import Node.Path (concat) as Path
import Partial.Unsafe (unsafePartial)
import Type.Prelude (Proxy(Proxy))

-- | Runs `ContractTest`s given a `ContractParams` value.
-- |
-- | This function can be used to interpret `TestPlanM ContractTest` in any
-- | environment.
-- |
-- | Tests are funded by the wallet in the supplied `Contract` environment.
-- | The `FilePath` parameter should point to a directory to store generated
-- | private keys (to make sure that no funds or on-chain state are lost).
runContractTestsWithKeyDir
  :: ContractParams
  -> FilePath
  -> TestPlanM ContractTest Unit
  -> TestPlanM (Aff Unit) Unit
runContractTestsWithKeyDir params backup = do
  mapTest \(ContractTest runContractTest) -> do
    withContractEnv params \env -> do
      keyWalletArr <- liftAff $ restoreWallets backup
      when (Array.length keyWalletArr > 0) do
        liftEffect $ info
          "Checking the backup wallets for leftover funds..."
        returnFunds backup env keyWalletArr Nothing false
      runContractTest \distr mkTest -> do
        addressStr /\ balance <- runContractInEnv env $ noLogs do
          address <-
            liftMaybe
              ( error
                  "Impossible happened: unable to get own address"
              ) =<< (getWalletAddresses <#> Array.head)
          balance <- unsafePartial $ getWalletBalance <#> fold
          pure $ Address.toBech32 address /\ balance
        let
          distrArray :: Array (Array UtxoAmount)
          distrArray = encodeDistribution distr

        -- Check if we have enough funds to distribute them (with some extra
        -- room). The fact our estimations are not precise is not a problem,
        -- because it is expected that test wallets have thousands of ADA.
        let
          distrTotalAmount :: BigInt
          distrTotalAmount = sum $ map (sum <<< map BigNum.toBigInt) distrArray

          minAdaRoughEstimation :: BigInt
          minAdaRoughEstimation = BigInt.fromInt 1_000_000

          feesToDistribute :: BigInt
          feesToDistribute =
            -- fees + minAda for all UTxOs, overestimated
            BigInt.fromInt 2_000_000 +
              minAdaRoughEstimation * BigInt.fromInt
                (sum (Array.length <$> distrArray))

        -- check if we have enough funds
        when
          ( BigNum.toBigInt (unwrap $ Value.getCoin balance) <= distrTotalAmount
              + feesToDistribute
          )
          do
            liftEffect $ throw $
              "The test engine cannot satisfy the requested ADA distribution "
                <> "because there are not enough funds left. \n\n"
                <> "Total required value is: "
                <> showLovelace distrTotalAmount
                <> " (estimated)\n"
                <> "Total available value is: "
                <> BigNum.toString (unwrap $ Value.getCoin balance)
                <> "\nThe test suite is using this address: "
                <> addressStr
                <> "\nFund it to continue."

        -- generate wallets
        privateKeys <- liftEffect $ for distrArray \_ -> PrivateKey.generate
        wallets <-
          liftMaybe
            ( error
                "Impossible happened: could not decode wallets. Please report as bug"
            )
            $ decodeWallets distr privateKeys

        let
          walletsArray :: Array KeyWallet
          walletsArray = keyWallets (pureProxy distr) wallets

          runTheContract :: Aff Unit
          runTheContract = runContractInEnv env { wallet = Nothing } do
            mkTest wallets

        -- We want to distinguish between successful and non-successful runs
        hasRunRef <- liftEffect $ Ref.new false

        if Array.null walletsArray then
          runTheContract
        else Aff.bracket
          ( do
              backupWallets backup env walletsArray
              fundWallets env walletsArray distrArray
          )
          -- Retry fund returning until success or timeout. Submission will fail if
          -- the node has seen the wallets utxos being spent previously, so retrying
          -- will allow the wallets utxos to eventually represent a spendable set
          ( \funds -> recovering returnFundsRetryPolicy ([ \_ _ -> pure true ])
              \_ -> do
                hasRun <- liftEffect $ Ref.read hasRunRef
                returnFunds backup env walletsArray (Just funds) hasRun
                runContractInEnv env (markAsInactive backup walletsArray)
          )
          \_ -> do
            runTheContract
            liftEffect $ Ref.write true hasRunRef
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
  flip parTraverse_ wallets \wallet -> do
    networkId <- asks _.networkId
    address <- liftAff $ Address.toBech32 <$> (unwrap wallet).address networkId
    let
      inactiveFlagFile = Path.concat [ backup, address, "inactive" ]
    liftAff $ writeTextFile UTF8 inactiveFlagFile $
      "This address was marked as inactive. "
        <> "The test engine assumes that there are no funds left on it. "
        <> "You can check it by inspecting the address on a chain explorer."

-- | Restore all wallets from the backups that are not marked as inactive.
-- | Normally, this function returns the wallets that have some funds of them
-- | left, e.g. those who were not emptied because the user hit Ctrl+C
restoreWallets :: FilePath -> Aff (Array KeyWallet)
restoreWallets backup = do
  walletDirs <- readdir backup <#> Array.filter (String.startsWith "addr")
  catMaybes <$> flip parTraverse walletDirs \walletDir -> do
    let
      paymentKeyFilePath = Path.concat
        [ backup, walletDir, "payment_signing_key" ]
      stakeKeyFilePath = Path.concat
        [ backup, walletDir, "stake_signing_key" ]
      inactiveFlagFile = Path.concat [ backup, walletDir, "inactive" ]
    -- Skip this wallet if it was marked as inactive
    liftEffect (exists inactiveFlagFile) >>= case _ of
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
        pure $ Just $ privateKeysToKeyWallet paymentKey mbStakeKey Nothing

-- | Save wallets to files in the backup directory for private keys
backupWallets :: FilePath -> ContractEnv -> Array KeyWallet -> Aff Unit
backupWallets backup env walletsArray =
  liftAff $ flip parTraverse_ walletsArray \wallet ->
    do
      payment <- getPrivatePaymentKey wallet
      mbStake <- getPrivateStakeKey wallet
      address <- liftAff $ Address.toBech32 <$> (unwrap wallet).address
        env.networkId
      let
        folder = Path.concat [ backup, address ]
      mkdir folder
      privatePaymentKeyToFile (Path.concat [ folder, "payment_signing_key" ])
        payment
      for mbStake $ privateStakeKeyToFile
        (Path.concat [ folder, "stake_signing_key" ])

-- | Create a transaction that builds a specific UTxO distribution on the wallets.
fundWallets
  :: ContractEnv -> Array KeyWallet -> Array (Array UtxoAmount) -> Aff BigNum
fundWallets env walletsArray distrArray = runContractInEnv env $ noLogs do
  logTrace' "Funding wallets"
  constraints <- liftAff $ flip foldMap (Array.zip walletsArray distrArray)
    \(wallet /\ walletDistr) -> flip foldMap walletDistr
      \value -> mustPayToKeyWallet wallet $
        Value.mkValue
          (wrap value)
          MultiAsset.empty
  txHash <- submitTxFromConstraints mempty constraints
  awaitTxConfirmed txHash
  let
    fundTotal =
      Array.foldl (\x y -> unsafeFromJust "fundWallets" $ BigNum.add x y)
        BigNum.zero $ join distrArray
  -- Use log so we can see, regardless of suppression
  info $ joinWith " "
    [ "Sent"
    , showLovelace $ BigNum.toBigInt fundTotal
    , "to test wallets"
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
        \the test suite. Most likely, the wallet ran out of funds. \
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
  :: FilePath
  -> ContractEnv
  -> Array KeyWallet
  -> Maybe BigNum
  -> Boolean
  -> Aff Unit
returnFunds backup env allWalletsArray mbFundTotal hasRun =
  runContractInEnv env $
    noLogs do
      nonEmptyWallets <- catMaybes <$>
        flip parTraverse allWalletsArray \wallet -> do
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
          info $ "Non-empty wallets found: " <> show
            (Array.length nonEmptyWallets)
          info $ "Returning ADA back to the main wallet before " <>
            "starting the test suite..."
        let utxos = nonEmptyWallets # map fst # Map.unions

        pkhs <- fold <$> for nonEmptyWallets
          (snd >>> flip withKeyWallet ownPaymentPubKeyHashes)

        let
          constraints = flip foldMap (Map.keys utxos) mustSpendPubKeyOutput
            <> foldMap mustBeSignedBy pkhs
          lookups = unspentOutputs utxos

        unbalancedTx /\ usedUtxos <- liftedE $ mkUnbalancedTxImpl lookups
          constraints
        balancedTx <- balanceTx unbalancedTx usedUtxos mempty
        balancedSignedTx <- Array.foldM
          (\tx wallet -> withKeyWallet wallet $ signTransaction tx)
          (wrap $ unwrap balancedTx)
          (nonEmptyWallets <#> snd)

        txHash <- submit balancedSignedTx
        awaitTxConfirmed txHash

        let
          (refundTotal :: BigInt) = Array.foldl
            ( \acc txorf -> acc + BigNum.toBigInt
                (unwrap $ Value.getCoin (txorf ^. _amount))
            )
            zero
            (Array.fromFoldable $ Map.values utxos)

        info $ joinWith " " $
          [ "Refunded"
          , showLovelace refundTotal
          ] <> maybe []
            ( \fundTotal ->
                [ "of"
                , showLovelace $ BigNum.toBigInt fundTotal
                , "from test wallets"
                ]
            )
            mbFundTotal
        for_ mbFundTotal \fundTotal -> do
          when (BigNum.toBigInt fundTotal == refundTotal && hasRun) do
            info $ "The test below didn't spend any ADA. Perhaps it does not "
              <> "need any funds to succeed. Consider using `noWallet` to "
              <> "skip funds distribution step"

showLovelace :: BigInt -> String
showLovelace n =
  if isAdaExact then BigInt.toString (n `div` million) <> " ADA"
  else BigInt.toString n <> " Lovelace"
  where
  million = BigInt.fromInt 1_000_000
  isAdaExact =
    (n `div` million) * million == n

-- | A helper function that abstracts away conversion between `KeyWallet` and
-- | its address and just gives us a `TxConstraints` value.
mustPayToKeyWallet
  :: forall (i :: Type) (o :: Type)
   . KeyWallet
  -> Value
  -> Aff TxConstraints
mustPayToKeyWallet wallet value = do
  kwPaymentKey <- getPrivatePaymentKey wallet
  kwMStakeKey <- getPrivateStakeKey wallet

  let
    convert = PublicKey.hash <<< PrivateKey.toPublicKey
    payment = over wrap convert $ kwPaymentKey
    mbStake = over wrap convert <$> kwMStakeKey
  pure $ maybe
    -- We don't use `mustPayToPubKey payment` to avoid the compile-time
    -- warning that is tied to it (it should not be propagated to
    -- `runContractTestWithKeyDir`)
    (singleton <<< MustPayToPubKeyAddress payment Nothing Nothing Nothing)
    (mustPayToPubKeyAddress payment)
    mbStake
    value
