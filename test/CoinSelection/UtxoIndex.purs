module Test.Ctl.CoinSelection.UtxoIndex where

import Prelude

import Control.Apply (lift2)
import Ctl.Internal.Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoMap
  )
import Ctl.Internal.Cardano.Types.Value (Value)
import Ctl.Internal.CoinSelection.UtxoIndex
  ( UtxoIndex
  , UtxoIndexInvariantStatus(InvariantHolds)
  )
import Ctl.Internal.CoinSelection.UtxoIndex
  ( buildUtxoIndex
  , checkUtxoIndexInvariants
  , emptyUtxoIndex
  , utxoIndexDeleteEntry
  , utxoIndexInsertEntry
  ) as UtxoIndex
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId(MainnetId)
  , baseAddressToAddress
  , paymentKeyHashStakeKeyHashAddress
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.OutputDatum (OutputDatum(NoOutputDatum))
import Ctl.Internal.Types.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  )
import Data.Generic.Rep (class Generic)
import Data.Map (empty) as Map
import Data.Map.Gen (genMap) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Data.UInt (fromInt) as UInt
import Effect.Aff (Aff)
import Mote (group, test)
import Test.QuickCheck (Result(Failed, Success)) as QuickCheck
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "UtxoIndex" do
    test "prop_buildUtxoIndex_empty" do
      UtxoIndex.buildUtxoIndex Map.empty `shouldEqual` UtxoIndex.emptyUtxoIndex

    test "prop_buildUtxoIndex_invariant" do
      quickCheck prop_buildUtxoIndex_invariant

    test "prop_utxoIndexInsertEntry_invariant" do
      quickCheck prop_utxoIndexInsertEntry_invariant

    test "prop_utxoIndexDeleteEntry_invariant" do
      quickCheck prop_utxoIndexDeleteEntry_invariant

prop_buildUtxoIndex_invariant :: ArbitraryUtxoMap -> QuickCheck.Result
prop_buildUtxoIndex_invariant =
  invariantHolds <<< UtxoIndex.buildUtxoIndex <<< unwrap

prop_utxoIndexInsertEntry_invariant
  :: ArbitraryTxUnspentOut -> ArbitraryUtxoIndex -> QuickCheck.Result
prop_utxoIndexInsertEntry_invariant entry utxoIndex =
  invariantHolds $
    UtxoIndex.utxoIndexInsertEntry (unwrap entry) (unwrap utxoIndex)

prop_utxoIndexDeleteEntry_invariant
  :: ArbitraryTxUnspentOut -> ArbitraryUtxoIndex -> QuickCheck.Result
prop_utxoIndexDeleteEntry_invariant entry utxoIndex =
  invariantHolds $
    UtxoIndex.utxoIndexDeleteEntry (unwrap entry) (unwrap utxoIndex)

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/9d73b57e23392e25148cfc8db560cb8f656cb56a/lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/UTxOIndexSpec.hs#L183
invariantHolds :: UtxoIndex -> QuickCheck.Result
invariantHolds utxoIndex =
  case UtxoIndex.checkUtxoIndexInvariants utxoIndex of
    InvariantHolds -> QuickCheck.Success
    status -> QuickCheck.Failed (show status)

--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

newtype ArbitraryUtxoIndex = ArbitraryUtxoIndex UtxoIndex

derive instance Newtype ArbitraryUtxoIndex _

instance Arbitrary ArbitraryUtxoIndex where
  arbitrary =
    (arbitrary :: Gen ArbitraryUtxoMap)
      <#> wrap <<< UtxoIndex.buildUtxoIndex <<< unwrap

newtype ArbitraryUtxoMap = ArbitraryUtxoMap UtxoMap

derive instance Generic ArbitraryUtxoMap _
derive instance Newtype ArbitraryUtxoMap _

instance Show ArbitraryUtxoMap where
  show = genericShow

instance Arbitrary ArbitraryUtxoMap where
  arbitrary = wrap <$> Map.genMap genTransactionInput genTransactionOutput

newtype ArbitraryTxUnspentOut =
  ArbitraryTxUnspentOut (TransactionInput /\ TransactionOutput)

derive instance Newtype ArbitraryTxUnspentOut _

instance Arbitrary ArbitraryTxUnspentOut where
  arbitrary = wrap <$> lift2 Tuple genTransactionInput genTransactionOutput

genTransactionInput :: Gen TransactionInput
genTransactionInput = unwrap <$> (arbitrary :: Gen ArbitraryTransactionInput)

genTransactionOutput :: Gen TransactionOutput
genTransactionOutput = unwrap <$> (arbitrary :: Gen ArbitraryTransactionOutput)

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

