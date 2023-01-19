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
import Data.Either (Either(Left), isLeft)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (try)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

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
  test "whole thing" do
    result <- liftEffect $ paginateArray (Just { limit: 3, page: 0 })
      [ 1, 2, 3 ]
    result `shouldEqual` [ 1, 2, 3 ]
  test "when limit is bigger than array length" do
    result <- liftEffect $ paginateArray (Just { limit: 100, page: 0 })
      [ 1, 2, 3 ]
    result `shouldEqual` [ 1, 2, 3 ]
  test "-1th page" do
    result <- liftEffect $ paginateArray (Just { limit: 1, page: -1 })
      [ 1, 2, 3 ]
    result `shouldEqual` []
  test "0th page of size 0" do
    eiResult <- liftEffect $ try $ paginateArray (Just { limit: 0, page: 0 })
      [ 1, 2, 3 ]
    eiResult `shouldSatisfy` isLeft
  test "Second page #1" do
    result <- liftEffect $ paginateArray (Just { limit: 1, page: 1 })
      [ 1, 2, 3 ]
    result `shouldEqual` [ 2 ]
  test "Second page #2" do
    result <- liftEffect $ paginateArray (Just { limit: 3, page: 1 })
      [ 1, 2, 3, 4, 5 ]
    result `shouldEqual` [ 4, 5 ]
  test "Second page #3" do
    result <- liftEffect $ paginateArray (Just { limit: 3, page: 0 })
      [ 1, 2, 3, 4, 5 ]
    result `shouldEqual` [ 1, 2, 3 ]
  test "Page number not in range" do
    result <- liftEffect $ catchPaginateError $ paginateArray
      (Just { limit: 3, page: 2 })
      [ 1, 2, 3 ]
    result `shouldEqual` (Left $ PaginateError 1)
