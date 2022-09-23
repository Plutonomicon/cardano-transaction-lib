module Ctl.Internal.QueryM.MinFee (calculateMinFee) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction
  , UtxoMap
  , _body
  , _collateral
  , _inputs
  )
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import Ctl.Internal.Cardano.Types.Value (Coin)
import Ctl.Internal.Helpers (liftM, liftedM)
import Ctl.Internal.QueryM (QueryM, getWalletAddresses)
import Ctl.Internal.QueryM.ProtocolParameters (getProtocolParameters)
import Ctl.Internal.QueryM.Utxos (getUtxo, getWalletCollateral)
import Ctl.Internal.Serialization.Address
  ( Address
  , addressPaymentCred
  , stakeCredentialToKeyHash
  )
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash)
import Ctl.Internal.Serialization.MinFee (calculateMinFeeCsl)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (fromFoldable)
import Data.Lens.Getter ((^.))
import Data.Map (empty, fromFoldable, lookup) as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set (fromFoldable, intersection, union) as Set
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Effect.Aff (error)

-- | Calculate `min_fee` using CSL with protocol parameters from Ogmios.
calculateMinFee :: Transaction -> QueryM Coin
calculateMinFee tx = do
  selfSigners <- getSelfSigners tx
  pparams <- getProtocolParameters
  calculateMinFeeCsl pparams selfSigners tx

getSelfSigners :: Transaction -> QueryM (Set Ed25519KeyHash)
getSelfSigners tx = do

  -- Get all tx input addresses
  let
    txInputs :: Set TransactionInput
    txInputs = tx ^. _body <<< _inputs

  (inUtxosAddrs :: Set Address) <- setFor txInputs $ \txInput ->
    liftedM (error $ "Couldn't get tx output for " <> show txInput) $
      (map <<< map) (_.address <<< unwrap) (getUtxo txInput)

  -- Get all tx output addressses
  let
    txCollats :: Set TransactionInput
    txCollats = Set.fromFoldable <<< fromMaybe [] $ tx ^. _body <<< _collateral

  walletCollats <- maybe Map.empty toUtxoMap <$> getWalletCollateral

  (inCollatAddrs :: Set Address) <- setFor txCollats
    ( \txCollat ->
        liftM (error $ "Couldn't get tx output for " <> show txCollat)
          $ (map (_.address <<< unwrap) <<< Map.lookup txCollat)
          $ walletCollats
    )

  -- Get own addressses
  (ownAddrs :: Set Address) <- Set.fromFoldable <$>
    (liftedM (error "Could not get own addresses") getWalletAddresses)

  -- Combine to get all self tx input addresses
  let
    txOwnAddrs = ownAddrs `Set.intersection`
      (inUtxosAddrs `Set.union` inCollatAddrs)

  -- Convert addresses to key hashes
  setFor txOwnAddrs $
    liftM (error "Could not convert address to key hash")
      <<< (addressPaymentCred >=> stakeCredentialToKeyHash)
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

  toUtxoMap :: Array TransactionUnspentOutput -> UtxoMap
  toUtxoMap = Map.fromFoldable <<< map
    (unwrap >>> \({ input, output }) -> input /\ output)
