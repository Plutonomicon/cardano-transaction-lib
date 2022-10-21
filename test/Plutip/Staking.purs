module Test.Ctl.Plutip.Staking
  ( suite
  ) where

import Prelude

import Contract.Address (ownPaymentPubKeyHash, ownStakePubKeyHash)
import Contract.Hashing (publicKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (liftedE, liftedM, wrapContract)
import Contract.PlutusData (unitRedeemer)
import Contract.Prelude (liftM)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Test.Plutip (runPlutipContract, withStakeKey)
import Contract.Transaction (balanceTx, signTransaction)
import Contract.TxConstraints
  ( mustDelegateStakePubKey
  , mustDelegateStakeScript
  , mustWithdrawStakePubKey
  )
import Contract.Wallet (withKeyWallet)
import Contract.Wallet.Key (keyWalletPrivateStakeKey, publicKeyFromPrivateKey)
import Control.Monad.Reader (asks)
import Ctl.Examples.AlwaysSucceeds (alwaysSucceedsScript)
import Ctl.Internal.Cardano.Types.Transaction (PoolPubKeyHash(PoolPubKeyHash))
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.QueryM.Pools
  ( getDelegationsAndRewards
  , getPoolIds
  , getPoolParameters
  )
import Ctl.Internal.Serialization.Address (keyHashCredential, rewardAddress)
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.TxConstraints
  ( mustDeregisterStakePubKey
  , mustRegisterPool
  , mustRegisterStakePubKey
  , mustRegisterStakeScript
  )
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Foldable (for_)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Plutip.Common (config, privateStakeKey)
import Test.Ctl.Plutip.Utils (submitAndLog)
import Test.Ctl.TestM (TestPlanM)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  let
    clusterConfig = config.clusterConfig
      { slotLength = Seconds 0.1
      , epochSize = UInt.fromInt 10
      }
    config' = config
      { clusterConfig = clusterConfig
      }

  group "Staking" do
    test "Register / Deregister stake key" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config' distribution $ flip withKeyWallet do
        alicePkh /\ aliceStakePkh <- do
          Tuple <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash <*>
            liftedM "Failed to get Stake PKH" ownStakePubKeyHash

        -- Register
        do
          let
            constraints = mustRegisterStakePubKey aliceStakePkh

            lookups :: Lookups.ScriptLookups Void
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh

          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

        -- Deregister stake key
        do
          let
            constraints = mustDeregisterStakePubKey aliceStakePkh

            lookups :: Lookups.ScriptLookups Void
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh

          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

    test "Register pool" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config' distribution \alice -> withKeyWallet alice do
        alicePkh /\ aliceStakePkh <- Tuple
          <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash
          <*> liftedM "Failed to get Stake PKH" ownStakePubKeyHash

        -- Register stake key
        do
          let
            constraints = mustRegisterStakePubKey aliceStakePkh

            lookups :: Lookups.ScriptLookups Void
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh

          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

        privateStakeKey <- liftM (error "Failed to get private stake key") $
          keyWalletPrivateStakeKey alice
        networkId <- asks $ unwrap >>> _.config >>> _.networkId

        -- Register pool
        do
          let
            rewardAccount =
              rewardAddress
                { network: networkId
                , paymentCred: keyHashCredential $ unwrap $ unwrap aliceStakePkh
                }
            vrfKeyHash = unsafePartial $ fromJust $ fromBytes
              $ unsafePartial
              $ fromJust
              $ hexToByteArray
                  "fbf6d41985670b9041c5bf362b5262cf34add5d265975de176d613ca05f37096"
            poolParams =
              { operator: PoolPubKeyHash $ publicKeyHash $
                  publicKeyFromPrivateKey
                    (unwrap privateStakeKey)
              , vrfKeyhash: vrfKeyHash -- needed to prove that the pool won the lottery
              , pledge: unsafePartial $ fromJust $ BigNum.fromBigInt $
                  BigInt.fromInt 1
              , cost: unsafePartial $ fromJust $ BigNum.fromBigInt $
                  BigInt.fromInt 1
              , margin:
                  { numerator: unsafePartial $ fromJust $ BigNum.fromBigInt $
                      BigInt.fromInt 1
                  , denominator: unsafePartial $ fromJust $ BigNum.fromBigInt $
                      BigInt.fromInt 1
                  }
              , rewardAccount
              , poolOwners:
                  [ publicKeyHash $ publicKeyFromPrivateKey
                      (unwrap privateStakeKey)
                  ]
              , relays: []
              , poolMetadata: Nothing
              }

            constraints = mustRegisterPool poolParams

            lookups :: Lookups.ScriptLookups Void
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh

          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

    test "Stake script registration" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000 * BigInt.fromInt 1_000
          , BigInt.fromInt 2_000_000_000 * BigInt.fromInt 1_000
          ]
      runPlutipContract config' distribution \alice ->
        withKeyWallet alice do
          alicePkh /\ aliceStakePkh <- Tuple
            <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash
            <*> liftedM "Failed to get Stake PKH" ownStakePubKeyHash
          validator <- alwaysSucceedsScript <#> unwrap >>> wrap

          -- Register stake script
          do
            let
              constraints =
                mustRegisterStakeScript validator

              lookups :: Lookups.ScriptLookups Void
              lookups = mempty

            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- List pools
          poolId <- do
            pools <- wrapContract getPoolIds
            logInfo' "Pool IDs:"
            logInfo' $ show pools
            for_ pools \poolId -> do
              logInfo' "Pool parameters"
              logInfo' <<< show =<< wrapContract (getPoolParameters poolId)
            liftM (error "unable to get any pools") (head pools)

          -- Delegate
          do
            let
              constraints =
                mustDelegateStakeScript validator unitRedeemer poolId

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

    test "Stake delegation to existing pool" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000 * BigInt.fromInt 1_000
          , BigInt.fromInt 2_000_000_000 * BigInt.fromInt 1_000
          ]
      runPlutipContract config' distribution \alice ->
        withKeyWallet alice do
          alicePkh /\ aliceStakePkh <- Tuple
            <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash
            <*> liftedM "Failed to get Stake PKH" ownStakePubKeyHash

          -- Register stake key
          do
            let
              constraints = mustRegisterStakePubKey aliceStakePkh

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- List pools
          poolId <- do
            pools <- wrapContract getPoolIds
            logInfo' "Pool IDs:"
            logInfo' $ show pools
            for_ pools \poolId -> do
              logInfo' "Pool parameters"
              logInfo' <<< show =<< wrapContract (getPoolParameters poolId)
            liftM (error "unable to get any pools") (head pools)

          -- Delegate
          do
            let
              constraints =
                mustDelegateStakePubKey aliceStakePkh poolId

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- Wait until rewards
          let
            -- No need for limit on number of retries, because we have a
            -- timeout for tests.
            waitUntilRewards = do
              mbDelegationsAndRewards <- wrapContract $
                getDelegationsAndRewards aliceStakePkh
              (mbDelegationsAndRewards <#> _.delegate) `shouldEqual` Just
                (Just poolId)
              case mbDelegationsAndRewards of
                Just dels@{ rewards } | unwrap rewards > zero ->
                  pure dels
                _ -> do
                  liftAff $ delay $ wrap 5000.0
                  waitUntilRewards

          { rewards: rewardsBefore } <- waitUntilRewards

          -- Withdraw
          do
            let
              constraints =
                mustWithdrawStakePubKey aliceStakePkh

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- Check rewards.
          -- Not going to deregister here, because the rewards are added too
          -- soon, and we can't deregister the stake key if there are rewards
          -- left.
          -- This will not happen in real life scenarios, because epoch are
          -- (usually) significantly longer.
          do
            { rewards: rewardsAfter } <-
              liftedM "Unable to get rewards" $ wrapContract $
                getDelegationsAndRewards aliceStakePkh
            rewardsAfter `shouldSatisfy` \after -> after < rewardsBefore
