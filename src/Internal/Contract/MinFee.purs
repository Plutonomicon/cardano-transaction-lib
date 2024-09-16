module Ctl.Internal.Contract.MinFee (calculateMinFee) where

import Prelude

import Cardano.Types
  ( Certificate
      ( StakeRegistration
      , StakeDeregistration
      , StakeDelegation
      , PoolRegistration
      , PoolRetirement
      , VoteDelegCert
      , StakeVoteDelegCert
      , StakeRegDelegCert
      , VoteRegDelegCert
      , StakeVoteRegDelegCert
      , AuthCommitteeHotCert
      , ResignCommitteeColdCert
      , RegDrepCert
      , UnregDrepCert
      , UpdateDrepCert
      )
  , Coin
  , Credential
  , Ed25519KeyHash
  , RewardAddress
  , Transaction
  , UtxoMap
  , Voter(Cc, Drep, Spo)
  , _body
  , _certs
  , _collateral
  , _inputs
  , _withdrawals
  )
import Cardano.Types.Address (Address, getPaymentCredential, getStakeCredential)
import Cardano.Types.Credential (asPubKeyHash)
import Cardano.Types.Credential (asPubKeyHash) as Credential
import Cardano.Types.TransactionBody (_votingProcedures)
import Cardano.Types.TransactionInput (TransactionInput)
import Ctl.Internal.Contract (getProtocolParameters)
import Ctl.Internal.Contract.Monad (Contract, getQueryHandle)
import Ctl.Internal.Contract.Wallet (getWalletAddresses)
import Ctl.Internal.Helpers (liftM, liftedM)
import Ctl.Internal.MinFee (calculateMinFeeCsl)
import Data.Array (fromFoldable, mapMaybe)
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (foldl)
import Data.Lens (view)
import Data.Lens.Getter ((^.))
import Data.Map (keys, lookup, values) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set
  ( difference
  , empty
  , fromFoldable
  , insert
  , intersection
  , mapMaybe
  , union
  ) as Set
import Data.Traversable (for)
import Data.UInt (UInt)
import Effect.Aff (error)
import Effect.Aff.Class (liftAff)

-- | Calculate the minimum transaction fee.
calculateMinFee :: Transaction -> UtxoMap -> UInt -> Contract Coin
calculateMinFee tx additionalUtxos refScriptsSize = do
  selfSigners <- getSelfSigners tx additionalUtxos
  pparams <- getProtocolParameters
  calculateMinFeeCsl pparams selfSigners tx refScriptsSize

-- | This function estimates the set of keys that must be used
-- | for signing to make the transaction valid for the network.
getSelfSigners :: Transaction -> UtxoMap -> Contract (Set Ed25519KeyHash)
getSelfSigners tx additionalUtxos = do
  queryHandle <- getQueryHandle

  -- Get all tx inputs and remove the additional ones.
  let
    txInputs :: Set TransactionInput
    txInputs =
      Set.difference
        (Set.fromFoldable $ tx ^. _body <<< _inputs)
        (Map.keys additionalUtxos)

    additionalUtxosAddrs :: Set Address
    additionalUtxosAddrs = Set.fromFoldable $
      (_.address <<< unwrap) <$> Map.values additionalUtxos

  (inUtxosAddrs :: Set Address) <- setFor txInputs $ \txInput ->
    liftedM (error $ "Couldn't get tx output for " <> show txInput)
      $ (map <<< map) (_.address <<< unwrap)
      $ case Map.lookup txInput additionalUtxos of
          Nothing ->
            liftAff (queryHandle.getUtxoByOref txInput <#> hush >>> join)
          Just utxo -> pure $ Just utxo

  let
    collateralInputs = tx ^. _body <<< _collateral

  (collateralAddresses :: Set Address) <-
    setFor (Set.fromFoldable collateralInputs) $ \txInput ->
      liftedM (error $ "Couldn't get tx output for " <> show txInput)
        $ (map <<< map) (_.address <<< unwrap)
        $ case Map.lookup txInput additionalUtxos of
            Nothing ->
              liftAff (queryHandle.getUtxoByOref txInput <#> hush >>> join)
            Just utxo -> pure $ Just utxo

  -- Get own addressses
  (ownAddrs :: Set Address) <- Set.fromFoldable <$> getWalletAddresses

  -- Combine to get all self tx input addresses
  let
    txOwnAddrs =
      (additionalUtxosAddrs `Set.union` ownAddrs) `Set.intersection`
        (inUtxosAddrs `Set.union` collateralAddresses)

  -- Extract payment pub key hashes from addresses.
  paymentPkhs <- map (Set.mapMaybe identity) $ setFor txOwnAddrs $ \addr -> do
    paymentCred <-
      liftM
        ( error $ "Could not extract payment credential from Address: " <> show
            addr
        ) $ getPaymentCredential addr
    pure $ asPubKeyHash $ unwrap paymentCred

  -- Extract stake pub key hashes from addresses
  let
    stakePkhs = Set.fromFoldable $
      (asPubKeyHash <<< unwrap <=< getStakeCredential) `mapMaybe`
        Array.fromFoldable txOwnAddrs

  -- Extract signers for certificates, withdrawals, and voting procedures
  let
    certsPkhs = getSignersForCerts tx
    withdrawalsPkhs = getSignersForWithdrawals tx
    votingProceduresPkhs = getSignersForVotingProcedures tx

  pure $ paymentPkhs <> stakePkhs <> certsPkhs <> withdrawalsPkhs
    <> votingProceduresPkhs
  where
  setFor
    :: forall (a :: Type) (b :: Type) (m :: Type -> Type)
     . Monad m
    => Ord a
    => Ord b
    => Set a
    -> (a -> m b)
    -> m (Set b)
  setFor txIns f = Set.fromFoldable <$> for (fromFoldable txIns) f

getSignersForCerts :: Transaction -> Set Ed25519KeyHash
getSignersForCerts = foldl worker Set.empty <<< view (_body <<< _certs)
  where
  worker :: Set Ed25519KeyHash -> Certificate -> Set Ed25519KeyHash
  worker acc =
    case _ of
      StakeRegistration _ -> acc
      StakeDeregistration cred -> addSigner $ unwrap cred
      StakeDelegation cred _ -> addSigner $ unwrap cred
      PoolRegistration poolParams -> Set.insert
        (unwrap (unwrap poolParams).operator)
        acc
      PoolRetirement { poolKeyHash } -> Set.insert (unwrap poolKeyHash) acc
      VoteDelegCert cred _ -> addSigner $ unwrap cred
      StakeVoteDelegCert cred _ _ -> addSigner $ unwrap cred
      StakeRegDelegCert cred _ _ -> addSigner $ unwrap cred
      VoteRegDelegCert cred _ _ -> addSigner $ unwrap cred
      StakeVoteRegDelegCert cred _ _ _ -> addSigner $ unwrap cred
      AuthCommitteeHotCert { coldCred } -> addSigner coldCred
      ResignCommitteeColdCert cred _ -> addSigner cred
      RegDrepCert cred _ _ -> addSigner cred
      UnregDrepCert cred _ -> addSigner cred
      UpdateDrepCert cred _ -> addSigner cred
    where
    addSigner :: Credential -> Set Ed25519KeyHash
    addSigner = maybe acc (flip Set.insert acc) <<< Credential.asPubKeyHash

getSignersForWithdrawals :: Transaction -> Set Ed25519KeyHash
getSignersForWithdrawals =
  foldl worker Set.empty <<< Map.keys <<< view (_body <<< _withdrawals)
  where
  worker :: Set Ed25519KeyHash -> RewardAddress -> Set Ed25519KeyHash
  worker acc =
    maybe acc (flip Set.insert acc) <<< Credential.asPubKeyHash <<< unwrap
      <<< _.stakeCredential

getSignersForVotingProcedures :: Transaction -> Set Ed25519KeyHash
getSignersForVotingProcedures =
  foldl worker Set.empty <<< Map.keys <<< unwrap
    <<< view (_body <<< _votingProcedures)
  where
  worker :: Set Ed25519KeyHash -> Voter -> Set Ed25519KeyHash
  worker acc =
    case _ of
      Cc cred -> addSigner cred
      Drep cred -> addSigner cred
      Spo poolKeyHash -> Set.insert poolKeyHash acc
    where
    addSigner :: Credential -> Set Ed25519KeyHash
    addSigner = maybe acc (flip Set.insert acc) <<< Credential.asPubKeyHash
