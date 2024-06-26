module Test.Ctl.Plutip.Staking
  ( main
  , suite
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types (PoolParams(PoolParams), UnitInterval(UnitInterval))
import Cardano.Types.BigInt as BigInt
import Cardano.Types.Credential
  ( Credential(ScriptHashCredential, PubKeyHashCredential)
  )
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusData (PlutusData(Integer))
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Address (getNetworkId)
import Contract.Backend.Ogmios (getPoolParameters)
import Contract.Hashing (publicKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Numeric.BigNum (fromInt, toBigInt) as BigNum
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.Prelude (liftM)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (NativeScript(ScriptPubkey, ScriptAny), applyArgs)
import Contract.Staking
  ( getPoolIds
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  )
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Plutip (runPlutipContract, withStakeKey)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Contract.Time (getCurrentEpoch)
import Contract.Transaction
  ( Epoch(Epoch)
  , PoolPubKeyHash(PoolPubKeyHash)
  , balanceTx
  , mkPoolPubKeyHash
  , signTransaction
  )
import Contract.TxConstraints
  ( DatumPresence(DatumWitness)
  , mustDelegateStakeNativeScript
  , mustDelegateStakePlutusScript
  , mustDelegateStakePubKey
  , mustDeregisterStakeNativeScript
  , mustDeregisterStakePlutusScript
  , mustDeregisterStakePubKey
  , mustPayToNativeScriptAddress
  , mustPayToScriptAddress
  , mustRegisterPool
  , mustRegisterStakePubKey
  , mustRegisterStakeScript
  , mustRetirePool
  , mustWithdrawStakeNativeScript
  , mustWithdrawStakePlutusScript
  , mustWithdrawStakePubKey
  )
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet
  ( ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  , withKeyWallet
  )
import Contract.Wallet.Key (keyWalletPrivateStakeKey, publicKeyFromPrivateKey)
import Ctl.Examples.AlwaysSucceeds (alwaysSucceedsScript)
import Ctl.Examples.Helpers (submitAndLog)
import Ctl.Examples.IncludeDatum (only42Script)
import Data.Array (head, (!!))
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff
  ( Aff
  , Milliseconds(Milliseconds)
  , cancelWith
  , delay
  , effectCanceler
  , launchAff
  )
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Mote (group, skip, test)
import Test.Ctl.Plutip.Common (config) as Common
import Test.Ctl.Plutip.Common (privateStakeKey)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Runner (defaultConfig)

main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 450_000.0, exit = true }
      suite

suite :: TestPlanM (Aff Unit) Unit
suite = do
  -- We must never select this pool, because it retires at the third epoch
  -- (this is Plutip internal knowledge)
  -- https://github.com/mlabs-haskell/plutip/blob/7f2d59abd911dd11310404863cdedb2886902ebf/src/Test/Plutip/Internal/Cluster.hs#L692
  retiringPoolId <- liftEffect $ liftM (error "unable to decode poolId bech32")
    $ mkPoolPubKeyHash
        "pool1rv7ur8r2hz02lly9q8ehtwcrcydl3m2arqmndvwcqsfavgaemt6"
  let
    -- A routine function that filters out retiring pool from the list of available
    -- pools
    selectPoolId :: Contract PoolPubKeyHash
    selectPoolId = do
      pools <- getPoolIds
      logInfo' "Pool IDs:"
      logInfo' $ show pools
      for_ pools \poolId -> do
        logInfo' "Pool parameters"
        logInfo' <<< show =<< getPoolParameters poolId
      liftM (error "unable to get any pools")
        (Array.filter (_ /= retiringPoolId) pools !! 1)
  group "Staking" do
    group "Stake keys: register & deregister" do
      test "PubKey" do
        let
          distribution = withStakeKey privateStakeKey
            [ BigNum.fromInt 1_000_000_000
            , BigNum.fromInt 2_000_000_000
            ]
        runPlutipContract config distribution $ flip withKeyWallet do
          alicePkh /\ aliceStakePkh <- do
            Tuple
              <$> liftedM "Failed to get PKH" (head <$> ownPaymentPubKeyHashes)
              <*>
                liftedM "Failed to get Stake PKH"
                  (join <<< head <$> ownStakePubKeyHashes)

          -- Register
          do
            let
              constraints = mustRegisterStakePubKey aliceStakePkh

              lookups :: Lookups.ScriptLookups
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh

            ubTx <- mkUnbalancedTx lookups constraints
            balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

          -- Deregister stake key
          do
            let
              constraints = mustDeregisterStakePubKey aliceStakePkh

              lookups :: Lookups.ScriptLookups
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh

            ubTx <- mkUnbalancedTx lookups constraints
            balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

      test "PlutusScript" do
        let
          distribution = withStakeKey privateStakeKey
            [ BigNum.fromInt 1_000_000_000
            , BigNum.fromInt 2_000_000_000
            ]
        runPlutipContract config distribution $ flip withKeyWallet do
          alicePkh /\ aliceStakePkh <- do
            Tuple
              <$> liftedM "Failed to get PKH" (head <$> ownPaymentPubKeyHashes)
              <*>
                liftedM "Failed to get Stake PKH"
                  (join <<< head <$> ownStakePubKeyHashes)
          validator1 <- alwaysSucceedsScript
          validator2 <- do
            only42 <- only42Script
            liftM (error "failed to apply args") do
              applyArgs only42 [ Integer $ BigInt.fromInt 42 ] # hush
          let
            validatorHash1 = PlutusScript.hash validator1
            validatorHash2 = PlutusScript.hash validator2

          -- Register
          do
            let
              constraints = mustRegisterStakeScript validatorHash1
                <> mustRegisterStakeScript validatorHash2

              lookups :: Lookups.ScriptLookups
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh

            ubTx <- mkUnbalancedTx lookups constraints
            balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

          -- Deregister stake key
          do
            let
              constraints =
                mustDeregisterStakePlutusScript validator1
                  unitRedeemer
                  <> mustDeregisterStakePlutusScript validator2
                    unitRedeemer

              lookups :: Lookups.ScriptLookups
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh

            ubTx <- mkUnbalancedTx lookups constraints
            balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

      test "NativeScript" do
        let
          distribution = withStakeKey privateStakeKey
            [ BigNum.fromInt 1_000_000_000
            , BigNum.fromInt 2_000_000_000
            ]
        runPlutipContract config distribution $ flip withKeyWallet do
          alicePkh /\ aliceStakePkh <- do
            Tuple
              <$> liftedM "Failed to get PKH" (head <$> ownPaymentPubKeyHashes)
              <*>
                liftedM "Failed to get Stake PKH"
                  (join <<< head <$> ownStakePubKeyHashes)
          let
            nativeScript = ScriptAny
              [ ScriptPubkey $ unwrap alicePkh ]
            stakeValidatorHash = NativeScript.hash nativeScript

          -- Register
          do
            let
              constraints = mustRegisterStakeScript stakeValidatorHash

              lookups :: Lookups.ScriptLookups
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh

            ubTx <- mkUnbalancedTx lookups constraints
            balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

          -- Deregister stake key
          do
            let
              constraints = mustDeregisterStakeNativeScript nativeScript

              lookups :: Lookups.ScriptLookups
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh

            ubTx <- mkUnbalancedTx lookups constraints
            balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

    test "Pool registration & retirement" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigNum.fromInt 1_000_000_000
          , BigNum.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> withKeyWallet alice do
        alicePkh /\ aliceStakePkh <- Tuple
          <$> liftedM "Failed to get PKH" (head <$> ownPaymentPubKeyHashes)
          <*> liftedM "Failed to get Stake PKH"
            (join <<< head <$> ownStakePubKeyHashes)

        -- Register stake key
        do
          let
            constraints = mustRegisterStakePubKey aliceStakePkh

            lookups :: Lookups.ScriptLookups
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh

          ubTx <- mkUnbalancedTx lookups constraints
          balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

        privateStakeKey <- liftM (error "Failed to get private stake key") $
          keyWalletPrivateStakeKey alice
        networkId <- getNetworkId
        let
          poolOperator = PoolPubKeyHash $ publicKeyHash $
            publicKeyFromPrivateKey (unwrap privateStakeKey)

        -- Register pool
        do
          vrfKeyHash <- liftM (error "Unable to decode VRFKeyHash") do
            hexToByteArray
              "fbf6d41985670b9041c5bf362b5262cf34add5d265975de176d613ca05f37096"
              >>= wrap >>> decodeCbor
          let
            rewardAccount =
              { networkId
              , stakeCredential: wrap $ PubKeyHashCredential $ unwrap
                  aliceStakePkh
              }

            poolParams = PoolParams
              { operator: poolOperator
              , vrfKeyhash: vrfKeyHash -- needed to prove that the pool won the lottery
              , pledge: BigNum.fromInt 1
              , cost: BigNum.fromInt 1
              , margin: UnitInterval
                  { numerator: BigNum.fromInt 1
                  , denominator: BigNum.fromInt 1
                  }
              , rewardAccount
              , poolOwners:
                  [ publicKeyHash $
                      publicKeyFromPrivateKey
                        (unwrap privateStakeKey)
                  ]
              , relays: []
              , poolMetadata: Nothing
              }

            constraints = mustRegisterPool poolParams

            lookups :: Lookups.ScriptLookups
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh

          ubTx <- mkUnbalancedTx lookups constraints
          balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

        -- List pools: the pool must appear in the list
        do
          pools <- getPoolIds
          pools `shouldSatisfy` Array.elem poolOperator

        currentEpoch <- getCurrentEpoch
        let
          -- NOTE: this is a source of flaky-ness
          -- (there's no guarantee that the tx will pass before the specified epoch).
          -- You will get something like this error if it's not the case:
          -- Error: `submit` call failed. Error from Ogmios: [{"wrongRetirementEpoch":{"currentEpoch":114,"firstUnreachableEpoch":1000114,"requestedEpoch":95}}]
          retirementEpoch :: Epoch
          retirementEpoch = Epoch (unwrap currentEpoch + UInt.fromInt 5)

        -- Retire pool
        do
          let
            constraints = mustRetirePool poolOperator retirementEpoch

            lookups :: Lookups.ScriptLookups
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh

          ubTx <- mkUnbalancedTx lookups constraints
          balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

        let
          waitEpoch :: Epoch -> Contract Epoch
          waitEpoch epoch = do
            epochNow <- getCurrentEpoch
            if unwrap epochNow >= unwrap epoch then pure epochNow
            else do
              liftAff $ delay $ Milliseconds 1000.0
              waitEpoch epoch

        void $ waitEpoch retirementEpoch

        -- List pools: the pool must not appear in the list
        do
          pools <- getPoolIds
          pools `shouldSatisfy` Array.notElem poolOperator

    skip $ test
      "Plutus Stake script: delegate to existing pool & withdraw rewards"
      do
        let
          distribution = withStakeKey privateStakeKey
            [ BigNum.fromInt 1_000_000_000
            , BigNum.fromInt 2_000_000_000
            ]
        runPlutipContract config distribution \alice ->
          withKeyWallet alice do
            alicePkh /\ aliceStakePkh <- Tuple
              <$> liftedM "Failed to get PKH" (head <$> ownPaymentPubKeyHashes)
              <*> liftedM "Failed to get Stake PKH"
                (join <<< head <$> ownStakePubKeyHashes)
            validator <- alwaysSucceedsScript
            let
              validatorHash = PlutusScript.hash validator

            -- Lock funds on the stake script
            do
              let
                constraints =
                  mustPayToScriptAddress validatorHash
                    (ScriptHashCredential validatorHash)
                    unitDatum
                    DatumWitness
                    $ lovelaceValueOf
                    $ BigNum.fromInt 1_000_000_000

                lookups :: Lookups.ScriptLookups
                lookups = mempty

              ubTx <- mkUnbalancedTx lookups constraints
              balanceTx ubTx Map.empty mempty >>= signTransaction >>=
                submitAndLog

            -- Register stake script
            do
              let
                constraints =
                  mustRegisterStakeScript validatorHash

                lookups :: Lookups.ScriptLookups
                lookups = mempty

              ubTx <- mkUnbalancedTx lookups constraints
              balanceTx ubTx Map.empty mempty >>= signTransaction >>=
                submitAndLog

            -- Select a pool
            poolId <- selectPoolId

            -- Delegate
            do
              let
                constraints =
                  mustDelegateStakePlutusScript validator unitRedeemer poolId

                lookups :: Lookups.ScriptLookups
                lookups =
                  Lookups.ownPaymentPubKeyHash alicePkh <>
                    Lookups.ownStakePubKeyHash aliceStakePkh
              ubTx <- mkUnbalancedTx lookups constraints
              balanceTx ubTx Map.empty mempty >>= signTransaction >>=
                submitAndLog

            -- Wait until rewards
            let
              -- No need for limit on number of retries, because we have a
              -- timeout for tests.
              waitUntilRewards = do
                mbDelegationsAndRewards <-
                  getValidatorHashDelegationsAndRewards validatorHash
                case mbDelegationsAndRewards of
                  Just dels@{ rewards }
                    | BigNum.toBigInt <<< unwrap <$> rewards > Just zero ->
                        pure dels
                  _ -> do
                    liftAff $ delay $ Milliseconds 5000.0
                    waitUntilRewards

            { rewards: rewardsBefore, delegate } <- waitUntilRewards
            delegate `shouldEqual` Just poolId

            -- Withdraw
            do
              let
                constraints =
                  mustWithdrawStakePlutusScript validator unitRedeemer

                lookups :: Lookups.ScriptLookups
                lookups =
                  Lookups.ownPaymentPubKeyHash alicePkh <>
                    Lookups.ownStakePubKeyHash aliceStakePkh
              ubTx <- mkUnbalancedTx lookups constraints
              balanceTx ubTx Map.empty mempty >>= signTransaction >>=
                submitAndLog

            -- Check rewards.
            -- Not going to deregister here, because the rewards are added too
            -- soon, and we can't deregister the stake key if there are rewards
            -- left.
            -- This will not happen in real life scenarios, because epoch are
            -- (usually) significantly longer.
            do
              { rewards: rewardsAfter } <- liftedM "Unable to get rewards" $
                getValidatorHashDelegationsAndRewards validatorHash
              rewardsAfter `shouldSatisfy` \after -> after < rewardsBefore

    skip $ test
      "Native Stake script: delegate to existing pool & withdraw rewards"
      do
        let
          distribution =
            [ BigNum.fromInt 1_000_000_000
            , BigNum.fromInt 2_000_000_000
            ] /\
              withStakeKey privateStakeKey
                [ BigNum.fromInt 1_000_000_000
                , BigNum.fromInt 2_000_000_000
                ]
        runPlutipContract config distribution \(alice /\ bob) -> do
          bobPkh /\ bobStakePkh <- withKeyWallet bob do
            Tuple
              <$> liftedM "Failed to get PKH" (head <$> ownPaymentPubKeyHashes)
              <*> liftedM "Failed to get Stake PKH"
                (join <<< head <$> ownStakePubKeyHashes)
          let
            nativeScript = ScriptAny
              [ ScriptPubkey $ unwrap bobStakePkh ]
            scriptHash = NativeScript.hash nativeScript

          -- Alice
          withKeyWallet alice do
            -- She locks funds on the stake script (no need for her to validate
            -- the script in order to do that)
            do
              let
                constraints =
                  mustPayToNativeScriptAddress
                    scriptHash
                    (ScriptHashCredential scriptHash)
                    $ lovelaceValueOf
                    $ BigNum.fromInt 1_000_000_000

                lookups :: Lookups.ScriptLookups
                lookups = mempty

              ubTx <- mkUnbalancedTx lookups constraints
              balanceTx ubTx Map.empty mempty >>= signTransaction >>=
                submitAndLog

            -- Alice registers stake script (again, no need to validate it)
            do
              let
                constraints =
                  mustRegisterStakeScript scriptHash

                lookups :: Lookups.ScriptLookups
                lookups = mempty

              ubTx <- mkUnbalancedTx lookups constraints
              balanceTx ubTx Map.empty mempty >>= signTransaction >>=
                submitAndLog

          -- Bob performs operations with the stake script that require his
          -- (and only his) signature.
          withKeyWallet bob do

            -- Select first pool
            poolId <- selectPoolId

            -- Delegate
            do
              let
                constraints =
                  mustDelegateStakeNativeScript nativeScript poolId

                lookups :: Lookups.ScriptLookups
                lookups =
                  Lookups.ownPaymentPubKeyHash bobPkh <>
                    Lookups.ownStakePubKeyHash bobStakePkh
              ubTx <- mkUnbalancedTx lookups constraints
              balanceTx ubTx Map.empty mempty >>= signTransaction >>=
                submitAndLog

            -- Wait until rewards
            let
              -- No need for limit on number of retries, because we have a
              -- timeout for tests.
              waitUntilRewards = do
                mbDelegationsAndRewards <-
                  getValidatorHashDelegationsAndRewards scriptHash
                case mbDelegationsAndRewards of
                  Just dels@{ rewards }
                    | BigNum.toBigInt <<< unwrap <$> rewards > Just zero ->
                        pure dels
                  _ -> do
                    liftAff $ delay $ Milliseconds 5000.0
                    waitUntilRewards

            { rewards: rewardsBefore, delegate } <- waitUntilRewards
            delegate `shouldEqual` Just poolId

            -- Withdraw
            do
              let
                constraints =
                  mustWithdrawStakeNativeScript nativeScript

                lookups :: Lookups.ScriptLookups
                lookups =
                  Lookups.ownPaymentPubKeyHash bobPkh <>
                    Lookups.ownStakePubKeyHash bobStakePkh
              ubTx <- mkUnbalancedTx lookups constraints
              balanceTx ubTx Map.empty mempty >>= signTransaction >>=
                submitAndLog

            -- Check rewards.
            -- Not going to deregister here, because the rewards are added too
            -- soon, and we can't deregister the stake key if there are rewards
            -- left.
            -- This will not happen in real life scenarios, because epoch are
            -- (usually) significantly longer.
            do
              { rewards: rewardsAfter } <- liftedM "Unable to get rewards" $
                getValidatorHashDelegationsAndRewards scriptHash
              rewardsAfter `shouldSatisfy` \after -> after < rewardsBefore

    skip $ test "PubKey: delegate to existing pool & withdraw rewards" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigNum.fromInt 1_000_000_000
          , BigNum.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice ->
        withKeyWallet alice do
          alicePkh /\ aliceStakePkh <- Tuple
            <$> liftedM "Failed to get PKH" (head <$> ownPaymentPubKeyHashes)
            <*> liftedM "Failed to get Stake PKH"
              (join <<< head <$> ownStakePubKeyHashes)

          -- Register stake key
          do
            let
              constraints = mustRegisterStakePubKey aliceStakePkh

              lookups :: Lookups.ScriptLookups
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- mkUnbalancedTx lookups constraints
            balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

          -- Select a pool ID
          poolId <- selectPoolId

          -- Delegate
          do
            let
              constraints =
                mustDelegateStakePubKey aliceStakePkh poolId

              lookups :: Lookups.ScriptLookups
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- mkUnbalancedTx lookups constraints
            balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

          -- Wait until rewards
          let
            -- No need for limit on number of retries, because we have a
            -- timeout for tests.
            waitUntilRewards = do
              mbDelegationsAndRewards <-
                getPubKeyHashDelegationsAndRewards $ unwrap aliceStakePkh
              case mbDelegationsAndRewards of
                Just dels@{ rewards }
                  | BigNum.toBigInt <<< unwrap <$> rewards > Just zero ->
                      pure dels
                _ -> do
                  liftAff $ delay $ Milliseconds 5000.0
                  waitUntilRewards

          { rewards: rewardsBefore, delegate: _ } <- waitUntilRewards

          -- TODO: why does the query layer return Nothing even though
          -- the rewards are received? Potential Ogmios bug, need to investigate
          -- delegate `shouldEqual` Just poolId

          -- Withdraw
          do
            let
              constraints =
                mustWithdrawStakePubKey aliceStakePkh

              lookups :: Lookups.ScriptLookups
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- mkUnbalancedTx lookups constraints
            balanceTx ubTx Map.empty mempty >>= signTransaction >>= submitAndLog

          -- Check rewards.
          -- Not going to deregister here, because the rewards are added too
          -- soon, and we can't deregister the stake key if there are rewards
          -- left.
          -- This will not happen in real life scenarios, because epoch are
          -- (usually) significantly longer.
          do
            { rewards: rewardsAfter } <-
              liftedM "Unable to get rewards"
                $ getPubKeyHashDelegationsAndRewards
                $ unwrap aliceStakePkh
            rewardsAfter `shouldSatisfy` \after -> after < rewardsBefore
  where
  config =
    Common.config
      { clusterConfig =
          Common.config.clusterConfig
            -- changing these constants breaks rewards
            -- https://github.com/mlabs-haskell/plutip/issues/149
            { slotLength = Seconds 0.05
            , epochSize = Just $ UInt.fromInt 80
            }
      }
