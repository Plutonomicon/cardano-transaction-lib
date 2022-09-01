module QueryM.MinFee (calculateMinFee) where

import Prelude

import Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , Transaction
  , _body
  , _collateral
  , _inputs
  )
import Cardano.Types.Value (Coin)
import Contract.Prelude (for, fromMaybe)
import Data.Array (fromFoldable)
import Data.Lens.Getter ((^.))
import Data.Map (fromFoldable, lookup, union) as Map
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set (fromFoldable, union) as Set
import Data.Tuple.Nested ((/\))
import Effect.Aff (error)
import Helpers (liftM)
import QueryM (QueryM)
import QueryM.ProtocolParameters (getProtocolParameters)
import QueryM.Utxos (getWalletCollateral, getWalletUtxos)
import Serialization.Address (addressPaymentCred, stakeCredentialToKeyHash)
import Serialization.Hash (Ed25519KeyHash)
import Serialization.MinFee (calculateMinFeeCsl)

-- | Calculate `min_fee` using CSL with protocol parameters from Ogmios.
calculateMinFee :: Transaction -> QueryM Coin
calculateMinFee tx = do
  selfSigners <- getSelfSigners tx
  pparams <- getProtocolParameters
  calculateMinFeeCsl pparams selfSigners tx

getSelfSigners :: Transaction -> QueryM (Set Ed25519KeyHash)
getSelfSigners tx = do

  walletCollats <- fromMaybe [] <$> getWalletCollateral
  walletUtxos <- unwrap <$>
    (liftM (error "CIP-30 wallet missing collateral") =<< getWalletUtxos)

  let
    walletCollatUtxoM = Map.fromFoldable $ walletCollats
      <#> unwrap >>>
        ( case _ of
            { input, output } -> input /\ output
        )
    utxosWithCollats = walletUtxos `Map.union` walletCollatUtxoM
    txCollats = (tx ^. _body <<< _collateral)
      # fromMaybe []
      # Set.fromFoldable
    txInputs = tx ^. _body <<< _inputs

    -- Traverse over inputs and collaterals to find required VKeyHashes that
    -- need to sign the Tx
    selfSigners = for (fromFoldable $ txInputs `Set.union` txCollats) \ti -> do
      TransactionOutput { address } <- Map.lookup ti utxosWithCollats
      kh <- (addressPaymentCred >=> stakeCredentialToKeyHash) address
      pure $ kh

  selfSignerSet <- pure (Set.fromFoldable <$> selfSigners) >>= liftM
    (error "failed to get self signers")

  -- logDebug' $ "self tx collats: " <> show txCollats
  -- logDebug' $ "self tx inputs: " <> show txInputs
  -- logDebug' $ "self walletCollats: " <> show walletCollats
  -- logDebug' $ "self selfSignerSet: " <> show selfSignerSet

  pure selfSignerSet
