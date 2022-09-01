module QueryM.MinFee (calculateMinFee) where

import Prelude

import Cardano.Types.Transaction
  ( Transaction
  , TransactionOutput(TransactionOutput)
  , _body
  , _collateral
  , _inputs
  )
import Cardano.Types.Value (Coin)
import Contract.Prelude (for, fromMaybe, note)
import Data.Array (fromFoldable)
import Data.Bifunctor (lmap)
import Data.Lens.Getter ((^.))
import Data.Map (Map)
import Data.Map (fromFoldable, keys, lookup, union) as Map
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set (fromFoldable, intersection, union) as Set
import Data.Tuple.Nested ((/\))
import Effect.Aff (error)
import Helpers (liftEither, liftM)
import QueryM (QueryM)
import QueryM.ProtocolParameters (getProtocolParameters)
import QueryM.Utxos (getWalletCollateral, getWalletUtxos)
import Serialization.Address (addressPaymentCred, stakeCredentialToKeyHash)
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

  walletCollats <- fromMaybe [] <$> getWalletCollateral
  walletUtxos <- unwrap <$>
    (liftM (error "CIP-30 wallet missing collateral") =<< getWalletUtxos)

  let

    walletCollatUtxoM :: Map TransactionInput TransactionOutput
    walletCollatUtxoM = Map.fromFoldable $ walletCollats
      <#> unwrap >>>
        ( case _ of
            { input, output } -> input /\ output
        )

    utxosWithCollats :: Map TransactionInput TransactionOutput
    utxosWithCollats = walletUtxos `Map.union` walletCollatUtxoM

    txCollats :: Set TransactionInput
    txCollats = Set.fromFoldable <<< fromMaybe [] $ tx ^. _body <<< _collateral

    txInputs :: Set TransactionInput
    txInputs = tx ^. _body <<< _inputs

    allSelfInputs = (txInputs `Set.intersection` Map.keys walletUtxos)
      `Set.union` txCollats

    -- Traverse over inputs and collaterals to find required VKeyHashes that
    -- need to sign the Tx
    selfSigners = for (fromFoldable allSelfInputs) \ti -> do
      TransactionOutput { address } <-
        note ("Couldn't find " <> show ti <> " in  " <> show utxosWithCollats) $
          Map.lookup ti utxosWithCollats
      kh <- note "Failed to convert to key hash" $
        (addressPaymentCred >=> stakeCredentialToKeyHash) address
      pure $ kh

  selfSignerSet <- pure (Set.fromFoldable <$> selfSigners) >>=
    (lmap error >>> liftEither)

  pure selfSignerSet
