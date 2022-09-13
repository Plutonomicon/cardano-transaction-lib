module QueryM.MinFee (calculateMinFee) where

import Prelude

import Cardano.Types.Transaction (Transaction, _body, _collateral, _inputs)
import Cardano.Types.Value (Coin)
import Data.Array (fromFoldable)
import Data.Lens.Getter ((^.))
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set (fromFoldable, intersection, union) as Set
import Data.Traversable (for)
import Effect.Aff (error)
import Helpers (liftM, liftedM)
import QueryM (QueryM, getWalletAddresses)
import QueryM.ProtocolParameters (getProtocolParameters)
import QueryM.Utxos (getUtxo)
import Serialization.Address
  ( Address
  , addressPaymentCred
  , stakeCredentialToKeyHash
  )
import Serialization.Hash (Ed25519KeyHash)
import Serialization.MinFee (calculateMinFeeCsl)
import Types.Transaction (TransactionInput)

-- | Calculate `min_fee` using CSL with protocol parameters from Ogmios.
calculateMinFee :: Transaction -> QueryM Coin
calculateMinFee tx = do
  selfSigners <- getSelfSigners tx
  pparams <- getProtocolParameters
  calculateMinFeeCsl pparams selfSigners tx

getSelfSigners :: Transaction -> QueryM (Set Ed25519KeyHash)
getSelfSigners tx = do

  let
    txInputs :: Set TransactionInput
    txInputs = tx ^. _body <<< _inputs

    txCollats :: Set TransactionInput
    txCollats = Set.fromFoldable <<< fromMaybe [] $ tx ^. _body <<< _collateral

    allInputs :: Set TransactionInput
    allInputs = Set.union txInputs txCollats

  (inUtxosKh :: Set Address) <- Set.fromFoldable <$>
    ( for (fromFoldable allInputs) $ \txInput -> do
        (utxoAddr :: Address) <-
          liftedM (error $ "Couldn't get tx output for " <> show txInput)
            $ (map <<< map) (_.address <<< unwrap) (getUtxo txInput)
        pure utxoAddr
    )

  (ownAddrs :: Set Address) <- Set.fromFoldable <$>
    (liftedM (error "Could not get own addresses") getWalletAddresses)

  let txOwnAddrs = Set.intersection inUtxosKh ownAddrs

  Set.fromFoldable
    <$> for (fromFoldable txOwnAddrs)
      ( (addressPaymentCred >=> stakeCredentialToKeyHash) >>>
          liftM (error "Could not convert address to key hash")
      )
