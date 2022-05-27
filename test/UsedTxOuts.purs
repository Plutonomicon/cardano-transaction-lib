module Test.UsedTxOuts (suite) where

import Prelude

import Cardano.Types.Transaction (Transaction)
import Control.Monad.Reader (runReaderT)
import Data.Array (any, singleton, uncons)
import Data.Foldable (all)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.UInt (UInt)
import Mote (test, group)
import Partial.Unsafe (unsafePartial)
import Test.Fixtures
  ( mkSampleTx
  , mkTxInput
  , txFixture1
  )
import Test.Spec.Assertions (shouldReturn)
import TestM (TestPlanM)
import Types.Transaction (TransactionHash)
import Types.UsedTxOuts
  ( isTxOutRefUsed
  , lockTransactionInputs
  , newUsedTxOuts
  , unlockTransactionInputs
  , unlockTxOutRefs
  )

buildSampleTransaction
  :: { tx :: Transaction
     , usedTxOutRefs ::
         Array { transactionId :: TransactionHash, index :: UInt }
     , unusedTxOutRefs ::
         Array { transactionId :: TransactionHash, index :: UInt }
     }
buildSampleTransaction =
  let
    usedTxOutRefs =
      [ mkTxInput
          { txId:
              "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
          , ix: 0
          }
      , mkTxInput
          { txId:
              "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
          , ix: 1
          }
      , mkTxInput
          { txId:
              "92c4f22371bd453aec9fe19ccebfbc88211ae854b5eab424bcd4c26d4c9754cc"
          , ix: 0
          }
      , mkTxInput
          { txId:
              "92c4f22371bd453aec9fe19ccebfbc88211ae854b5eab424bcd4c26d4c9754cc"
          , ix: 1
          }
      , mkTxInput
          { txId:
              "92c4f22371bd453aec9fe19ccebfbc88211ae854b5eab424bcd4c26d4c9754cc"
          , ix: 2
          }
      , mkTxInput
          { txId:
              "c9e9c15d4f16a7948d3736c93aa79034621d51dccc4df5d31c7d34aa0b3240cc"
          , ix: 1
          }
      ]
    unusedTxOutRefs =
      [ mkTxInput
          { txId:
              "a69fdfbdab33eeb05143bf3b05591679fb104157fa68fe3f4268631aedf5e607"
          , ix: 0
          }
      , mkTxInput
          { txId:
              "a69fdfbdab33eeb05143bf3b05591679fb104157fa68fe3f4268631aedf5e607"
          , ix: 1
          }
      , mkTxInput
          { txId:
              "a2cb07ae236c4d8ee549d1bf3ad3818d691a5a7c53c1065276bfbb0ac332ccd7"
          , ix: 0
          }
      ]
  in
    { tx: mkSampleTx txFixture1 (_ { inputs = usedTxOutRefs })
    , usedTxOutRefs: unwrap <$> usedTxOutRefs
    , unusedTxOutRefs: unwrap <$> unusedTxOutRefs
    }

suite :: TestPlanM Unit
suite =
  group "UsedTxOuts API tests" do

    let
      { tx, usedTxOutRefs, unusedTxOutRefs } = buildSampleTransaction
      anyTxOutsLocked txos = any identity <$> traverse isTxOutRefUsed txos
      allTxOutsLocked txos = all identity <$> traverse isTxOutRefUsed txos

    test "UsedTxOuts cache properly locks and unlocks tx txouts" $ do
      newUsedTxOuts >>= runReaderT do
        -- starts empty
        anyTxOutsLocked (usedTxOutRefs <> unusedTxOutRefs) `shouldReturn` false
        -- lock
        _ <- lockTransactionInputs tx
        allTxOutsLocked usedTxOutRefs `shouldReturn` true
        anyTxOutsLocked unusedTxOutRefs `shouldReturn` false

        -- unlock
        unlockTransactionInputs tx
        anyTxOutsLocked (usedTxOutRefs <> unusedTxOutRefs) `shouldReturn` false

    test "UsedTxOuts cache properly locks and unlock selected txouts" $ do
      newUsedTxOuts >>= runReaderT do
        -- starts empty
        anyTxOutsLocked (usedTxOutRefs <> unusedTxOutRefs) `shouldReturn` false

        -- lock
        _ <- lockTransactionInputs tx
        allTxOutsLocked usedTxOutRefs `shouldReturn` true
        anyTxOutsLocked unusedTxOutRefs `shouldReturn` false

        -- unlock unused
        unlockTxOutRefs unusedTxOutRefs
        allTxOutsLocked usedTxOutRefs `shouldReturn` true
        anyTxOutsLocked unusedTxOutRefs `shouldReturn` false

        -- unlock used
        unlockTxOutRefs usedTxOutRefs
        anyTxOutsLocked (usedTxOutRefs <> unusedTxOutRefs) `shouldReturn` false

        -- lock
        _ <- lockTransactionInputs tx
        allTxOutsLocked usedTxOutRefs `shouldReturn` true

        -- unlock 'head'
        let { head, tail } = unsafePartial $ fromJust (uncons usedTxOutRefs)
        unlockTxOutRefs $ singleton head
        -- head unlocked
        isTxOutRefUsed head `shouldReturn` false
        -- tail remains locked
        allTxOutsLocked tail `shouldReturn` true
        anyTxOutsLocked unusedTxOutRefs `shouldReturn` false
