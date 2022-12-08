module Test.Ctl.CoinSelection.UtxoIndex where

import Prelude

import Control.Apply (lift2)
import Ctl.Internal.BalanceTx.CoinSelection
  ( Asset(..)
  , BundleCategory(..)
  , UtxoIndex
  )
import Ctl.Internal.BalanceTx.CoinSelection as UtxoIndex
import Ctl.Internal.Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoMap
  )
import Ctl.Internal.Cardano.Types.Value (Value)
import Ctl.Internal.Cardano.Types.Value (getAssetQuantity, valueAssets) as Value
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId(MainnetId)
  , baseAddressToAddress
  , paymentKeyHashStakeKeyHashAddress
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.OutputDatum (OutputDatum(NoOutputDatum))
import Ctl.Internal.Types.Transaction
  ( TransactionHash(..)
  , TransactionInput(..)
  )
import Data.Array (all) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Foldable (all, and, length) as Foldable
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.HashMap (lookup, toArrayBy) as HashMap
import Data.Lens (Lens')
import Data.Lens.Getter ((^.))
import Data.Map (empty, lookup, toUnfoldable) as Map
import Data.Map.Gen (genMap) as Map
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromInt) as UInt
import Effect.Aff (Aff)
import Mote (group, test)
import Test.QuickCheck (Result(Failed, Success)) as QuickCheck
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Undefined (undefined)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "UtxoIndex" do
    test "prop_buildUtxoIndex_empty" do
      UtxoIndex.buildUtxoIndex Map.empty `shouldEqual` UtxoIndex.emptyUtxoIndex

    test "prop_buildUtxoIndex_invariant" do
      quickCheck prop_buildUtxoIndex_invariant

prop_buildUtxoIndex_invariant :: ArbitraryUtxoMap -> QuickCheck.Result
prop_buildUtxoIndex_invariant =
  invariantHolds <<< UtxoIndex.buildUtxoIndex <<< unwrap

--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

newtype ArbitraryUtxoMap = ArbitraryUtxoMap UtxoMap

derive instance Generic ArbitraryUtxoMap _
derive instance Newtype ArbitraryUtxoMap _

instance Show ArbitraryUtxoMap where
  show = genericShow

instance Arbitrary ArbitraryUtxoMap where
  arbitrary = wrap <$> Map.genMap (unwrap <$> oref) (unwrap <$> txOutput)
    where
    oref :: Gen ArbitraryTransactionInput
    oref = arbitrary

    txOutput :: Gen ArbitraryTransactionOutput
    txOutput = arbitrary

newtype ArbitraryTransactionInput =
  ArbitraryTransactionInput TransactionInput

derive instance Newtype ArbitraryTransactionInput _

instance Arbitrary ArbitraryTransactionInput where
  arbitrary = wrap <$> lift2 mkTxInput arbitrary arbitrary
    where
    mkTxInput :: TransactionHash -> Int -> TransactionInput
    mkTxInput transactionId index =
      TransactionInput
        { transactionId
        , index: UInt.fromInt index
        }

newtype ArbitraryTransactionOutput =
  ArbitraryTransactionOutput TransactionOutput

derive instance Newtype ArbitraryTransactionOutput _

instance Arbitrary ArbitraryTransactionOutput where
  arbitrary = wrap <$> lift2 mkTxOutput arbitrary arbitrary
    where
    mkTxOutput :: ArbitraryAddress -> Value -> TransactionOutput
    mkTxOutput address amount =
      TransactionOutput
        { address: unwrap address
        , amount
        , datum: NoOutputDatum
        , scriptRef: Nothing
        }

newtype ArbitraryAddress = ArbitraryAddress Address

derive instance Newtype ArbitraryAddress _

instance Arbitrary ArbitraryAddress where
  arbitrary =
    wrap <<< baseAddressToAddress <$>
      lift2 (paymentKeyHashStakeKeyHashAddress MainnetId) arbitrary arbitrary

--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L614
data UtxoIndexInvariantStatus
  = InvariantHolds
  -- ^ Indicates a successful check of the invariants.
  | InvariantUtxoIndexIncomplete
  -- ^ Indicates that the `UtxoIndex` is missing one or more entries.
  | InvariantUtxoIndexNonMinimal

-- ^ Indicates that the `UtxoIndex` has one or more unnecessary entries.

derive instance Generic UtxoIndexInvariantStatus _

instance Show UtxoIndexInvariantStatus where
  show = genericShow

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/9d73b57e23392e25148cfc8db560cb8f656cb56a/lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/UTxOIndexSpec.hs#L183
invariantHolds :: UtxoIndex -> QuickCheck.Result
invariantHolds utxoIndex =
  case checkUtxoIndexInvariants utxoIndex of
    InvariantHolds -> QuickCheck.Success
    status -> QuickCheck.Failed (show status)

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L632
checkUtxoIndexInvariants :: UtxoIndex -> UtxoIndexInvariantStatus
checkUtxoIndexInvariants utxoIndex
  | not (checkUtxoIndexComplete utxoIndex) =
      InvariantUtxoIndexIncomplete
  | not (checkUtxoIndexMinimal utxoIndex) =
      InvariantUtxoIndexNonMinimal
  | otherwise =
      InvariantHolds

-- | Check that every entry from the map of all utxos is properly indexed.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L682
checkUtxoIndexComplete :: UtxoIndex -> Boolean
checkUtxoIndexComplete utxoIndex =
  Array.all hasEntries (Map.toUnfoldable $ utxoIndex ^. UtxoIndex._utxos)
  where
  hasEntries :: TransactionInput /\ TransactionOutput -> Boolean
  hasEntries (oref /\ out) =
    case UtxoIndex.categorizeUtxoEntry out of
      BundleWithNoAssets ->
        UtxoIndex._indexSingletons `hasEntryForAsset` AssetLovelace
      BundleWithOneAsset asset ->
        UtxoIndex._indexPairs `hasEntryForAsset` AssetLovelace
          && UtxoIndex._indexSingletons `hasEntryForAsset` Asset asset
      BundleWithTwoAssets asset0 asset1 ->
        UtxoIndex._indexAnyWith `hasEntryForAsset` AssetLovelace
          && UtxoIndex._indexPairs `hasEntryForAsset` Asset asset0
          && UtxoIndex._indexPairs `hasEntryForAsset` Asset asset1
      BundleWithMultipleAssets assets ->
        UtxoIndex._indexAnyWith `hasEntryForAsset` AssetLovelace &&
          flip Foldable.all assets \asset ->
            (UtxoIndex._indexAnyWith `hasEntryForAsset` Asset asset)
    where
    hasEntryForAsset
      :: Lens' UtxoIndex (HashMap Asset UtxoMap) -> Asset -> Boolean
    hasEntryForAsset getter asset =
      maybe false (eq out) $
        (Map.lookup oref =<< HashMap.lookup asset (utxoIndex ^. getter))

-- | Check that every indexed entry is required by some entry in the map of all
-- | utxos.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L715
checkUtxoIndexMinimal :: UtxoIndex -> Boolean
checkUtxoIndexMinimal utxoIndex =
  UtxoIndex._indexSingletons `testEntriesWith` txOutputHasOneAsset
    && UtxoIndex._indexPairs `testEntriesWith` txOutputHasTwoAssetsWith
    && UtxoIndex._indexAnyWith `testEntriesWith` txOutputHasAsset
  where
  testEntriesWith
    :: Lens' UtxoIndex (HashMap Asset UtxoMap)
    -> (TransactionOutput -> Asset -> Boolean)
    -> Boolean
  testEntriesWith subset test' =
    utxoIndex ^. subset
      # HashMap.toArrayBy Tuple
      # Array.all \(asset /\ utxos) ->
          Array.all (entryMatches (flip test' asset)) (Map.toUnfoldable utxos)
    where
    entryMatches
      :: (TransactionOutput -> Boolean)
      -> TransactionInput /\ TransactionOutput
      -> Boolean
    entryMatches test'' (oref /\ txOutput) =
      maybe false test'' $
        Map.lookup oref (utxoIndex ^. UtxoIndex._utxos)

  txOutputHasOneAsset :: TransactionOutput -> Asset -> Boolean
  txOutputHasOneAsset txOutput AssetLovelace =
    txOutputAssetCount txOutput == zero
  txOutputHasOneAsset txOutput asset =
    txOutputHasAsset txOutput asset && txOutputAssetCount txOutput == one

  txOutputHasTwoAssetsWith :: TransactionOutput -> Asset -> Boolean
  txOutputHasTwoAssetsWith txOutput AssetLovelace =
    txOutputAssetCount txOutput == one
  txOutputHasTwoAssetsWith txOutput asset =
    txOutputHasAsset txOutput asset
      && txOutputAssetCount txOutput == BigInt.fromInt 2

  txOutputHasAsset :: TransactionOutput -> Asset -> Boolean
  txOutputHasAsset _ AssetLovelace = true
  txOutputHasAsset (TransactionOutput { amount }) (Asset asset) =
    Value.getAssetQuantity asset amount >= one

  txOutputAssetCount :: TransactionOutput -> BigInt
  txOutputAssetCount =
    Foldable.length <<< Value.valueAssets <<< _.amount <<< unwrap

