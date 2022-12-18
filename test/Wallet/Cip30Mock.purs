module Test.Ctl.Wallet.Cip30Mock
  ( suite
  ) where

import Prelude

import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Wallet.Cip30Mock
  ( PaginateError(PaginateError)
  , catchPaginateError
  , paginateArray
  )
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Cip30Mock" do
    group "Pagination" do
      testPagination

testPagination :: TestPlanM (Aff Unit) Unit
testPagination = do
  test "No pagination" do
    result <- liftEffect $ paginateArray Nothing [ 1 ]
    result `shouldEqual` [ 1 ]
  test "Paginate whole" do
    result <- liftEffect $ paginateArray (Just { limit: 3, page: 0 })
      [ 1, 2, 3 ]
    result `shouldEqual` [ 1, 2, 3 ]
  test "Paginate when limit is bigger than array length" do
    result <- liftEffect $ paginateArray (Just { limit: 100, page: 0 })
      [ 1, 2, 3 ]
    result `shouldEqual` [ 1, 2, 3 ]
  test "Paginate second page" do
    result <- liftEffect $ paginateArray (Just { limit: 1, page: 1 })
      [ 1, 2, 3 ]
    result `shouldEqual` [ 2 ]
  test "Paginate too big page" do
    result <- liftEffect $ catchPaginateError $ paginateArray
      (Just { limit: 3, page: 2 })
      [ 1, 2, 3 ]
    result `shouldEqual` (Left $ PaginateError 1)
