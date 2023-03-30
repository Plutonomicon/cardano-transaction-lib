module Test.Ctl.UsedTxOuts (suite) where

import Prelude

import Contract.Config (Storage, mkRefStorage)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (_body, _inputs)
import Ctl.Internal.Types.Transaction (TransactionInput(TransactionInput))
import Ctl.Internal.UsedTxOuts (get) as UsedTxOuts
import Ctl.Internal.UsedTxOuts (lockTransactionInputs, unlockAllBy)
import Data.Array as Array
import Data.Lens ((.~))
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Mote (group, test)
import Test.Ctl.Fixtures (mkTxInput)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "UsedTxOuts" do
    test "Locking Tx inputs works" do
      storage <- liftEffect mkRefStorage
      let
        inputs =
          [ mkTxInput
              { txId:
                  "0000000000000000000000000000000000000000000000000000000000000000"
              , ix: 0
              }
          , mkTxInput
              { txId:
                  "0000000000000000000000000000000000000000000000000000000000000000"
              , ix: 1
              }
          , mkTxInput
              { txId:
                  "1111111111111111111111111111111111111111111111111111111111111111"
              , ix: 0
              }
          , mkTxInput
              { txId:
                  "1111111111111111111111111111111111111111111111111111111111111111"
              , ix: 1
              }
          , mkTxInput
              { txId:
                  "1111111111111111111111111111111111111111111111111111111111111111"
              , ix: 2
              }
          , mkTxInput
              { txId:
                  "2222222222222222222222222222222222222222222222222222222222222222"
              , ix: 1
              }
          ]
      res <- liftEffect $ lockTransactionInputs 0 storage
        (mempty # _body <<< _inputs .~ Set.fromFoldable inputs)
      res `shouldEqual` true
    test "Locking same inputs twice fails" do
      storage <- liftEffect mkRefStorage
      let
        inputs1 =
          [ mkTxInput
              { txId:
                  "0000000000000000000000000000000000000000000000000000000000000000"
              , ix: 0
              }
          ]
        inputs2 =
          [ mkTxInput
              { txId:
                  "0000000000000000000000000000000000000000000000000000000000000000"
              , ix: 0
              }
          ]
      res1 <- liftEffect $ lockTransactionInputs 0 storage
        (mempty # _body <<< _inputs .~ Set.fromFoldable inputs1)
      res1 `shouldEqual` true
      res2 <- liftEffect $ lockTransactionInputs 0 storage
        (mempty # _body <<< _inputs .~ Set.fromFoldable inputs2)
      res2 `shouldEqual` false
    test "Unlocking (unlockAllBy) works" do
      storage <- liftEffect mkRefStorage
      let
        appInstance0 = 0
        appInstance1 = 1
        inputs0 =
          [ mkTxInput
              { txId:
                  "0000000000000000000000000000000000000000000000000000000000000000"
              , ix: 0
              }
          ]
        inputs1 =
          [ mkTxInput
              { txId:
                  "0000000000000000000000000000000000000000000000000000000000000000"
              , ix: 1
              }
          ]
      res1 <- liftEffect $ lockTransactionInputs appInstance0 storage
        (mempty # _body <<< _inputs .~ Set.fromFoldable inputs0)
      res1 `shouldEqual` true
      res2 <- liftEffect $ lockTransactionInputs appInstance1 storage
        (mempty # _body <<< _inputs .~ Set.fromFoldable inputs1)
      res2 `shouldEqual` true
      liftEffect (getInputs storage) >>= shouldEqual (inputs0 <> inputs1)
      liftEffect $ unlockAllBy storage appInstance0
      liftEffect (getInputs storage) >>= shouldEqual (inputs1)
      liftEffect $ unlockAllBy storage appInstance1
      liftEffect (getInputs storage) >>= shouldEqual []

getInputs :: Storage -> Effect (Array TransactionInput)
getInputs storage = Array.concat <$> do
  UsedTxOuts.get storage <#> Map.toUnfoldable >>> map \(txHash /\ locks) ->
    Array.fromFoldable (Map.keys locks) <#> \ix -> TransactionInput
      { transactionId: txHash
      , index: ix
      }
