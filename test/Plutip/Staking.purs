module Test.Ctl.Plutip.Staking
  ( suite
  ) where

import Prelude

import Contract.Address (ownPaymentPubKeyHash, ownStakePubKeyHash)
import Contract.Monad (liftedE, liftedM, wrapContract)
import Contract.Prelude (liftM)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Test.Plutip (runPlutipContract, withStakeKey)
import Contract.Transaction (balanceAndSignTxE)
import Contract.TxConstraints (mustDelegateStake)
import Contract.Wallet (withKeyWallet)
import Contract.Wallet.Key (keyWalletPrivateStakeKey)
import Control.Monad.Reader (asks)
import Ctl.Internal.Cardano.Types.Transaction (PoolPubKeyHash(PoolPubKeyHash))
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.QueryM.Pools
  ( getDelegationsAndRewards
  , getPoolIds
  , getPoolParameters
  )
import Ctl.Internal.Serialization (publicKeyFromPrivateKey, publicKeyHash)
import Ctl.Internal.Serialization.Address
  ( RewardAddress
  , keyHashCredential
  , rewardAddress
  )
import Ctl.Internal.Serialization.Types (VRFKeyHash)
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.TxConstraints
  ( mustRegisterPool
  , mustRegisterStakePubKey
  )
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Foldable (for_)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error)
import Mote (group, only, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Plutip.Common (config, privateStakeKey)
import Test.Ctl.Plutip.Utils (submitAndLog)
import Test.Ctl.TestM (TestPlanM)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Staking" do
    test "mustRegisterStakePubKey" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution $ flip withKeyWallet do
        alicePkh /\ aliceStakePkh <- do
          Tuple <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash <*>
            liftedM "Failed to get Stake PKH" ownStakePubKeyHash
        let
          constraints = mustRegisterStakePubKey aliceStakePkh

          lookups :: Lookups.ScriptLookups Void
          lookups =
            Lookups.ownPaymentPubKeyHash alicePkh <>
              Lookups.ownStakePubKeyHash aliceStakePkh
        ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
        bsTx <-
          liftedE $ balanceAndSignTxE ubTx
        submitAndLog bsTx
    test "mustRegisterPool" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> withKeyWallet alice do
        alicePkh /\ aliceStakePkh <- Tuple
          <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash
          <*>
            liftedM "Failed to get Stake PKH" ownStakePubKeyHash
        liftEffect $ Console.log $ show aliceStakePkh
        do
          -- register stake key
          let
            constraints = mustRegisterStakePubKey aliceStakePkh

            lookups :: Lookups.ScriptLookups Void
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh
          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          bsTx <-
            liftedE $ balanceAndSignTxE ubTx
          submitAndLog bsTx

        privateStakeKey <- liftM (error "Failed to get private stake key") $
          keyWalletPrivateStakeKey alice
        networkId <- asks $ unwrap >>> _.config >>> _.networkId

        -- rewardAddr <- liftM (error "failed to get reward address") $
        --    payPubKeyHashRewardAddress networkId alicePkh
        let
          rewardAddr' =
            rewardAddress
              { network: networkId
              , paymentCred: keyHashCredential $ unwrap $ unwrap aliceStakePkh
              }
        --   rewardAddressCsl =
        --     fromPlutusAddressWithNetworkTag (AddressWithNetworkTag { address: rewardAddr, networkId: networkId })
        -- rewardAccount <- liftM (error "failed to get reward address 2") $ rewardAddressFromAddress rewardAddressCsl
        let
          vrfKeyHash = unsafePartial $ fromJust $ fromBytes
            $ unsafePartial
            $ fromJust
            $ hexToByteArray
                "fbf6d41985670b9041c5bf362b5262cf34add5d265975de176d613ca05f37096"
          poolParams =
            { operator: PoolPubKeyHash $ publicKeyHash $ publicKeyFromPrivateKey
                (unwrap privateStakeKey)
            , vrfKeyhash: vrfKeyHash :: VRFKeyHash -- needed to prove that the pool won the lottery
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
            , rewardAccount: rewardAddr' :: RewardAddress
            , poolOwners:
                [ publicKeyHash $ publicKeyFromPrivateKey
                    (unwrap privateStakeKey)
                ]
            , relays: []
            , poolMetadata: Nothing -- Just $ PoolMetadata { url: URL "http://example.com"
            --             , hash:
            }

          constraints = mustRegisterPool poolParams

          lookups :: Lookups.ScriptLookups Void
          lookups =
            Lookups.ownPaymentPubKeyHash alicePkh <>
              Lookups.ownStakePubKeyHash aliceStakePkh
        ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
        bsTx <-
          liftedE $ balanceAndSignTxE ubTx
        submitAndLog bsTx

    only $ test "Stake delegation to existing pool" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000 * BigInt.fromInt 1_000
          , BigInt.fromInt 2_000_000_000 * BigInt.fromInt 1_000
          ]
      runPlutipContract config { suppressLogs = false } distribution \alice ->
        withKeyWallet alice do
          alicePkh /\ aliceStakePkh <- Tuple
            <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash
            <*>
              liftedM "Failed to get Stake PKH" ownStakePubKeyHash
          do
            let
              constraints = mustRegisterStakePubKey aliceStakePkh

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            bsTx <-
              liftedE $ balanceAndSignTxE ubTx
            submitAndLog bsTx

          do
            dels <- wrapContract $ getDelegationsAndRewards aliceStakePkh
            liftEffect $ Console.log $ show dels
          pools <- wrapContract getPoolIds
          for_ pools \poolId -> do
            params <- wrapContract $ getPoolParameters poolId
            liftEffect $ Console.log $ show params
          poolId <- liftM (error "unable to get any pools") (head pools)
          let
            constraints =
              mustDelegateStake aliceStakePkh poolId

            lookups :: Lookups.ScriptLookups Void
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh
          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          bsTx <- liftedE $ balanceAndSignTxE ubTx
          submitAndLog bsTx
          liftAff $ delay $ wrap 100000.0
          do
            dels <- wrapContract $ getDelegationsAndRewards aliceStakePkh
            dels.delegate `shouldEqual` Just poolId
            liftEffect $ Console.log $ show dels
