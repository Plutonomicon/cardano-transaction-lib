module Test.Ctl.Testnet.Staking
  ( main
  , suite
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Transaction.Builder
  ( CredentialWitness(NativeScriptCredential, PlutusScriptCredential)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(WithdrawRewards, IssueCertificate, Pay)
  )
import Cardano.Types
  ( Certificate
      ( StakeDelegation
      , StakeRegistration
      , StakeDeregistration
      , PoolRetirement
      , PoolRegistration
      )
  , Credential(ScriptHashCredential, PubKeyHashCredential)
  , OutputDatum(OutputDatum)
  , PlutusData(Integer)
  , PoolParams(PoolParams)
  , TransactionOutput(TransactionOutput)
  , UnitInterval(UnitInterval)
  , _body
  , _certs
  )
import Cardano.Types.BigInt as BigInt
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.PrivateKey as PrivateKey
import Cardano.Types.PublicKey as PublicKey
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Cardano.Wallet.Key (PrivateStakeKey)
import Contract.Address (getNetworkId, mkAddress)
import Contract.Backend.Ogmios (getPoolParameters)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Numeric.BigNum (fromInt, toBigInt) as BigNum
import Contract.Prelude (liftM)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Scripts (NativeScript(ScriptPubkey, ScriptAny))
import Contract.Staking
  ( getPoolIds
  , getPubKeyHashDelegationsAndRewards
  , getStakeCredentialDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  )
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Testnet (defaultTestnetConfig, runTestnetContract)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Contract.Time (getCurrentEpoch)
import Contract.Transaction
  ( Epoch(Epoch)
  , PoolPubKeyHash(PoolPubKeyHash)
  , balanceTx
  , buildTx
  , signTransaction
  )
import Contract.Value (lovelaceValueOf)
import Contract.Wallet
  ( ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  , withKeyWallet
  )
import Contract.Wallet.Key (getPrivateStakeKey)
import Control.Bind (bindFlipped)
import Ctl.Examples.AlwaysSucceeds (alwaysSucceedsScript)
import Ctl.Examples.Helpers (submitAndLog)
import Ctl.Examples.IncludeDatum (only42Script)
import Ctl.Internal.Test.UtxoDistribution
  ( InitialUTxOs
  , InitialUTxOsWithStakeKey(InitialUTxOsWithStakeKey)
  )
import Data.Array (head)
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Lens ((.~))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Seconds(Seconds))
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
import Effect.Exception (error)
import Mote (group, skip, test)
import Test.Ctl.Testnet.Common (privateStakeKey)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Runner (defaultConfig)

main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 90_000.0, exit = true }
      suite

suite :: TestPlanM (Aff Unit) Unit
suite = do
  let
    -- A routine function that selects a pool from the list of available pools
    selectPoolId :: Contract PoolPubKeyHash
    selectPoolId = do
      pools <- getPoolIds
      logInfo' "Pool IDs:"
      logInfo' $ show pools
      for_ pools \poolId -> do
        logInfo' "Pool parameters"
        logInfo' <<< show =<< getPoolParameters poolId
      liftM (error "unable to get any pools") $ head pools
  group "Staking" do
    group "Stake keys: register & deregister" do
      test "PubKey" do
        let
          distribution = withStakeKey privateStakeKeyForDist
            [ BigNum.fromInt 1_000_000_000
            , BigNum.fromInt 2_000_000_000
            ]
        runTestnetContract config distribution $ flip withKeyWallet do
          aliceStakePkh <- liftedM "Failed to get Stake PKH"
            (join <<< head <$> ownStakePubKeyHashes)

          -- Register
          do
            let
              transaction = Transaction.empty
                # _body <<< _certs .~
                    [ StakeRegistration $ wrap $ PubKeyHashCredential $ unwrap
                        aliceStakePkh
                    ]

            balanceTx transaction Map.empty mempty >>= signTransaction >>=
              submitAndLog

          -- Deregister stake key
          do
            tx <- buildTx
              [ IssueCertificate
                  ( StakeDeregistration $ wrap $ PubKeyHashCredential $ unwrap
                      aliceStakePkh
                  )
                  Nothing
              ]

            balanceTx tx Map.empty mempty >>= signTransaction >>= submitAndLog

      test "PlutusScript" do
        let
          distribution = withStakeKey privateStakeKeyForDist
            [ BigNum.fromInt 1_000_000_000
            , BigNum.fromInt 2_000_000_000
            ]
        runTestnetContract config distribution $ flip withKeyWallet do
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
              tx =
                Transaction.empty
                  # _body <<< _certs .~
                      [ StakeRegistration $ wrap $ ScriptHashCredential
                          validatorHash1
                      , StakeRegistration $ wrap $ ScriptHashCredential
                          validatorHash2
                      ]

            balanceTx tx Map.empty mempty >>= signTransaction >>= submitAndLog

          -- Deregister stake key
          do
            let
              plan =
                [ IssueCertificate
                    ( StakeDeregistration $ wrap $ ScriptHashCredential $
                        PlutusScript.hash validator1
                    )
                    $ Just
                    $ PlutusScriptCredential (ScriptValue validator1)
                        RedeemerDatum.unit
                , IssueCertificate
                    ( StakeDeregistration $ wrap $ ScriptHashCredential $
                        PlutusScript.hash validator2
                    )
                    $ Just
                    $ PlutusScriptCredential (ScriptValue validator2)
                        RedeemerDatum.unit
                ]

            tx <- buildTx plan
            balanceTx tx Map.empty mempty >>= signTransaction >>= submitAndLog

      test "NativeScript" do
        let
          distribution = withStakeKey privateStakeKeyForDist
            [ BigNum.fromInt 1_000_000_000
            , BigNum.fromInt 2_000_000_000
            ]
        runTestnetContract config distribution $ flip withKeyWallet do
          alicePkh <- liftedM "Failed to get PKH"
            (head <$> ownPaymentPubKeyHashes)
          let
            nativeScript = ScriptAny
              [ ScriptPubkey $ unwrap alicePkh ]
            stakeValidatorHash = NativeScript.hash nativeScript

          -- Register
          do
            let
              tx =
                Transaction.empty
                  # _body <<< _certs .~
                      [ StakeRegistration $ wrap $ ScriptHashCredential
                          stakeValidatorHash
                      ]

            balanceTx tx Map.empty mempty >>= signTransaction >>= submitAndLog

          -- Deregister stake key
          do
            let
              credential = wrap $ ScriptHashCredential $ NativeScript.hash
                nativeScript
              plan =
                [ IssueCertificate (StakeDeregistration credential)
                    $ Just
                    $ NativeScriptCredential
                    $ ScriptValue nativeScript
                ]

            tx <- buildTx plan
            balanceTx tx Map.empty mempty >>= signTransaction >>= submitAndLog

    test "Pool registration & retirement" do
      let
        distribution = withStakeKey privateStakeKeyForDist
          [ BigNum.fromInt 1_000_000_000
          , BigNum.fromInt 2_000_000_000
          ]
      runTestnetContract config distribution \alice -> withKeyWallet alice do
        aliceStakePkh <- liftedM "Failed to get Stake PKH"
          (join <<< head <$> ownStakePubKeyHashes)

        -- Register stake key
        do
          let
            credential = wrap $ PubKeyHashCredential $ unwrap aliceStakePkh

            plan =
              [ IssueCertificate (StakeRegistration credential) Nothing ]
          tx <- buildTx plan
          balanceTx tx Map.empty mempty >>= signTransaction >>= submitAndLog

        kwMStakeKey <- liftAff $ getPrivateStakeKey alice
        privateStakeKey <- liftM (error "Failed to get private stake key")
          kwMStakeKey
        networkId <- getNetworkId
        let
          poolOperator = PoolPubKeyHash $ PublicKey.hash $
            PrivateKey.toPublicKey (unwrap privateStakeKey)

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
                  [ PublicKey.hash $
                      PrivateKey.toPublicKey
                        (unwrap privateStakeKey)
                  ]
              , relays: []
              , poolMetadata: Nothing
              }

            tx =
              Transaction.empty # _body <<< _certs .~
                [ PoolRegistration poolParams ]

          balanceTx tx Map.empty mempty >>= signTransaction >>= submitAndLog

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
            tx =
              Transaction.empty # _body <<< _certs .~
                [ PoolRetirement
                    { poolKeyHash: poolOperator, epoch: retirementEpoch }
                ]
          balanceTx tx Map.empty mempty >>= signTransaction >>= submitAndLog

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
          distribution = withStakeKey privateStakeKeyForDist
            [ BigNum.fromInt 1_000_000_000
            , BigNum.fromInt 2_000_000_000
            ]
        runTestnetContract config distribution \alice ->
          withKeyWallet alice do
            validator <- alwaysSucceedsScript
            let
              validatorHash = PlutusScript.hash validator

            -- Lock funds on the stake script
            do
              address <- mkAddress (wrap $ ScriptHashCredential validatorHash)
                (Just $ wrap $ ScriptHashCredential $ validatorHash)
              let
                plan =
                  [ Pay $ TransactionOutput
                      { address
                      , datum: Just $ OutputDatum PlutusData.unit
                      , amount: lovelaceValueOf
                          $ BigNum.fromInt 1_000_000_000
                      , scriptRef: Nothing
                      }
                  ]
              tx <- buildTx plan
              balanceTx tx Map.empty mempty >>= signTransaction >>=
                submitAndLog

            -- Register stake script
            do
              let
                credential = wrap $ ScriptHashCredential validatorHash
              tx <- buildTx
                [ IssueCertificate (StakeRegistration credential)
                    $ Just
                    $ PlutusScriptCredential
                        (ScriptValue validator)
                        RedeemerDatum.unit
                ]
              balanceTx tx Map.empty mempty >>= signTransaction >>=
                submitAndLog

            -- Select a pool
            poolId <- selectPoolId

            -- Delegate
            do
              let
                credential = wrap $ ScriptHashCredential $ PlutusScript.hash
                  validator
                plan =
                  [ IssueCertificate (StakeDeregistration credential)
                      $ Just
                      $ PlutusScriptCredential
                          (ScriptValue validator)
                          RedeemerDatum.unit
                  ]
              tx <- buildTx plan
              balanceTx tx Map.empty mempty >>= signTransaction >>=
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
                credential = wrap $ ScriptHashCredential $ PlutusScript.hash
                  validator

              rewardsAmount <- liftedM "Unable to get rewards" $
                getStakeCredentialDelegationsAndRewards credential <#>
                  bindFlipped _.rewards
              let
                plan =
                  [ WithdrawRewards credential rewardsAmount
                      $ Just
                          ( PlutusScriptCredential (ScriptValue validator)
                              RedeemerDatum.unit
                          )
                  ]
              tx <- buildTx plan
              balanceTx tx Map.empty mempty >>= signTransaction >>=
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
              withStakeKey privateStakeKeyForDist
                [ BigNum.fromInt 1_000_000_000
                , BigNum.fromInt 2_000_000_000
                ]
        runTestnetContract config distribution \(alice /\ bob) -> do
          bobStakePkh <- withKeyWallet bob do
            liftedM "Failed to get Stake PKH"
              (join <<< head <$> ownStakePubKeyHashes)
          let
            nativeScript = ScriptAny
              [ ScriptPubkey $ unwrap bobStakePkh ]
            scriptHash = NativeScript.hash nativeScript
            credential = wrap $ ScriptHashCredential $ NativeScript.hash
              nativeScript

          -- Alice
          withKeyWallet alice do
            -- She locks funds on the stake script (no need for her to validate
            -- the script in order to do that)
            do
              address <- mkAddress (wrap $ ScriptHashCredential scriptHash)
                (Just $ wrap $ ScriptHashCredential scriptHash)
              let
                plan =
                  [ Pay $ TransactionOutput
                      { address
                      , datum: Just $ OutputDatum PlutusData.unit
                      , amount: lovelaceValueOf
                          $ BigNum.fromInt 1_000_000_000
                      , scriptRef: Nothing
                      }
                  ]
              tx <- buildTx plan
              balanceTx tx Map.empty mempty >>= signTransaction >>=
                submitAndLog

            -- Alice registers stake script (again, no need to validate it)
            do
              tx <- buildTx
                [ IssueCertificate (StakeRegistration credential)
                    $ Just
                    $ NativeScriptCredential
                    $ ScriptValue nativeScript
                ]
              balanceTx tx Map.empty mempty >>= signTransaction >>=
                submitAndLog

          -- Bob performs operations with the stake script that require his
          -- (and only his) signature.
          withKeyWallet bob do

            -- Select first pool
            poolId <- selectPoolId

            -- Delegate
            do
              tx <- buildTx
                [ IssueCertificate
                    ( StakeDelegation
                        ( wrap $ ScriptHashCredential $ NativeScript.hash
                            nativeScript
                        )
                        poolId
                    )
                    $ Just
                    $ NativeScriptCredential
                    $ ScriptValue nativeScript
                ]
              balanceTx tx Map.empty mempty >>= signTransaction >>=
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
              rewardsAmount <- liftedM "Unable to get rewards" $
                getStakeCredentialDelegationsAndRewards credential <#>
                  bindFlipped _.rewards
              let
                plan =
                  [ WithdrawRewards credential rewardsAmount
                      $ Just (NativeScriptCredential (ScriptValue nativeScript))
                  ]
              tx <- buildTx plan
              balanceTx tx Map.empty mempty >>= signTransaction >>=
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
        distribution = withStakeKey privateStakeKeyForDist
          [ BigNum.fromInt 1_000_000_000
          , BigNum.fromInt 2_000_000_000
          ]
      runTestnetContract config distribution \alice ->
        withKeyWallet alice do
          aliceStakePkh <- liftedM "Failed to get Stake PKH"
            (join <<< head <$> ownStakePubKeyHashes)

          -- Register stake key
          do
            let
              tx = Transaction.empty
                # _body <<< _certs .~
                    [ StakeRegistration $ wrap $ PubKeyHashCredential $ unwrap $
                        aliceStakePkh
                    ]

            balanceTx tx Map.empty mempty >>= signTransaction >>= submitAndLog

          -- Select a pool ID
          poolId <- selectPoolId

          -- Delegate
          do
            tx <- buildTx
              [ IssueCertificate
                  ( StakeDelegation
                      (wrap $ PubKeyHashCredential $ unwrap aliceStakePkh)
                      poolId
                  )
                  Nothing
              ]

            balanceTx tx Map.empty mempty >>= signTransaction >>= submitAndLog

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
              credential = wrap $ PubKeyHashCredential $ unwrap aliceStakePkh

            rewardsAmount <- liftedM "Unable to get rewards" $
              getStakeCredentialDelegationsAndRewards credential <#>
                bindFlipped _.rewards
            let
              plan =
                [ WithdrawRewards credential rewardsAmount Nothing ]
            tx <- buildTx plan
            balanceTx tx Map.empty mempty >>= signTransaction >>= submitAndLog

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
    defaultTestnetConfig
      { clusterConfig =
          defaultTestnetConfig.clusterConfig
            { slotLength = Seconds 0.05
            , epochSize = Just $ UInt.fromInt 80
            }
      -- , suppressLogs = false
      }

withStakeKey :: PrivateStakeKey -> InitialUTxOs -> InitialUTxOsWithStakeKey
withStakeKey = InitialUTxOsWithStakeKey

privateStakeKeyForDist :: PrivateStakeKey
privateStakeKeyForDist = privateStakeKey
