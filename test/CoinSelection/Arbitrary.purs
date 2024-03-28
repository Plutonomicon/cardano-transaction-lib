module Test.Ctl.CoinSelection.Arbitrary where

import Prelude

import Cardano.Types
  ( Address
  , NetworkId(MainnetId)
  , TransactionHash
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , UtxoMap
  , Value
  )
import Control.Apply (lift2)
import Ctl.Internal.BalanceTx.CoinSelection (SelectionState, fromIndexFiltered)
import Ctl.Internal.CoinSelection.UtxoIndex (UtxoIndex)
import Ctl.Internal.CoinSelection.UtxoIndex (buildUtxoIndex) as UtxoIndex
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map.Gen (genMap) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Data.UInt (fromInt) as UInt
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)

--------------------------------------------------------------------------------
-- ArbitraryUtxoIndex
--------------------------------------------------------------------------------

newtype ArbitraryUtxoIndex = ArbitraryUtxoIndex UtxoIndex

derive instance Newtype ArbitraryUtxoIndex _

instance Arbitrary ArbitraryUtxoIndex where
  arbitrary =
    (arbitrary :: Gen ArbitraryUtxoMap)
      <#> wrap <<< UtxoIndex.buildUtxoIndex <<< unwrap

--------------------------------------------------------------------------------
-- ArbitraryUtxoMap
--------------------------------------------------------------------------------

newtype ArbitraryUtxoMap = ArbitraryUtxoMap UtxoMap

derive instance Generic ArbitraryUtxoMap _
derive instance Newtype ArbitraryUtxoMap _

instance Show ArbitraryUtxoMap where
  show = genericShow

instance Arbitrary ArbitraryUtxoMap where
  arbitrary = wrap <$> Map.genMap genTransactionInput genTransactionOutput

--------------------------------------------------------------------------------
-- ArbitraryTxUnspentOut
--------------------------------------------------------------------------------

newtype ArbitraryTxUnspentOut =
  ArbitraryTxUnspentOut (TransactionInput /\ TransactionOutput)

derive instance Newtype ArbitraryTxUnspentOut _

instance Arbitrary ArbitraryTxUnspentOut where
  arbitrary = wrap <$> lift2 Tuple genTransactionInput genTransactionOutput

genTransactionInput :: Gen TransactionInput
genTransactionInput = unwrap <$> (arbitrary :: Gen ArbitraryTransactionInput)

genTransactionOutput :: Gen TransactionOutput
genTransactionOutput = unwrap <$> (arbitrary :: Gen ArbitraryTransactionOutput)

--------------------------------------------------------------------------------
-- ArbitraryTransactionInput
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- ArbitraryTransactionOutput
--------------------------------------------------------------------------------

newtype ArbitraryTransactionOutput =
  ArbitraryTransactionOutput TransactionOutput

derive instance Newtype ArbitraryTransactionOutput _

derive newtype instance Arbitrary ArbitraryTransactionOutput
--------------------------------------------------------------------------------
-- ArbitrarySelectionState
--------------------------------------------------------------------------------

newtype ArbitrarySelectionState = ArbitrarySelectionState SelectionState

derive instance Newtype ArbitrarySelectionState _

instance Arbitrary ArbitrarySelectionState where
  arbitrary = ArbitrarySelectionState <$>
    lift2 fromIndexFiltered
      (arbitrary :: Gen (TransactionInput -> Boolean))
      (unwrap <$> (arbitrary :: Gen ArbitraryUtxoIndex))

--------------------------------------------------------------------------------
-- ArbitraryMap
--------------------------------------------------------------------------------

newtype ArbitraryMap (k :: Type) (v :: Type) = ArbitraryMap (Map k v)

derive instance Newtype (ArbitraryMap k v) _

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (ArbitraryMap k v) where
  arbitrary = ArbitraryMap <$> Map.genMap arbitrary arbitrary
