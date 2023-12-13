module Ctl.Internal.Contract.MinFee (calculateMinFee) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction
  , UtxoMap
  , _body
  , _collateral
  , _inputs
  )
import Ctl.Internal.Cardano.Types.Value (Coin)
import Ctl.Internal.Contract (getProtocolParameters)
import Ctl.Internal.Contract.Monad (Contract, getQueryHandle)
import Ctl.Internal.Contract.Wallet (getWalletAddresses)
import Ctl.Internal.Helpers (liftM, liftedM)
import Ctl.Internal.Serialization.Address
  ( Address
  , addressPaymentCred
  , addressStakeCred
  , stakeCredentialToKeyHash
  )
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash)
import Ctl.Internal.Serialization.MinFee (calculateMinFeeCsl)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (fromFoldable, mapMaybe)
import Data.Array as Array
import Data.Either (hush)
import Data.Lens (non)
import Data.Lens.Getter ((^.))
import Data.Map (keys, lookup, values) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set (difference, fromFoldable, intersection, mapMaybe, union) as Set
import Data.Traversable (for)
import Effect.Aff (error)
import Effect.Aff.Class (liftAff)

-- | Calculate `min_fee` using CSL with protocol parameters from Ogmios.
calculateMinFee :: Transaction -> UtxoMap -> Contract Coin
calculateMinFee tx additionalUtxos = do
  selfSigners <- getSelfSigners tx additionalUtxos
  pparams <- getProtocolParameters
  calculateMinFeeCsl pparams selfSigners tx

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
        (tx ^. _body <<< _inputs)
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
    collateralInputs = tx ^. _body <<< _collateral <<< non []

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
        ) $ addressPaymentCred addr
    pure $ stakeCredentialToKeyHash paymentCred

  -- Extract stake pub key hashes from addresses
  let
    stakePkhs = Set.fromFoldable $
      (stakeCredentialToKeyHash <=< addressStakeCred) `mapMaybe`
        Array.fromFoldable txOwnAddrs

  pure $ paymentPkhs <> stakePkhs
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
