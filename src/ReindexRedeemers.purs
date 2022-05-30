-- | This module implements functionality to reindex redeemers to be used after
-- | balancing when all inputs have been inserted to the array.
module ReindexRedeemers
  ( ReindexErrors(..)
  , reindexSpentScriptRedeemers
  , reindexSpentScriptRedeemers'
  ) where

import Prelude

import Cardano.Types.Transaction (Redeemer(Redeemer)) as T
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Data.Array (elemIndex)
import Data.BigInt (fromInt)
import Data.Either (Either(Right), note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Helpers (liftEither)
import QueryM (QueryM)
import Types.RedeemerTag (RedeemerTag(Spend))
import Types.Transaction (TransactionInput)

-- | The only error should be impossible but we keep this here in case the user
-- | decides to call this function at some point where inputs have been
-- | accidentally deleted and/or not after balancing
data ReindexErrors = CannotGetTxOutRefIndexForRedeemer T.Redeemer

derive instance Generic ReindexErrors _

instance Show ReindexErrors where
  show = genericShow

type RedeemersTxIn = T.Redeemer /\ Maybe TransactionInput

-- | Reindex the `Spend` redeemers. Since we insert to an ordered array, we must
-- | reindex the redeemers with such inputs. This must be crucially called after
-- | balancing when all inputs are in place so they cannot be reordered.
reindexSpentScriptRedeemers
  :: Array TransactionInput
  -> Array RedeemersTxIn
  -> QueryM (Either ReindexErrors (Array T.Redeemer))
reindexSpentScriptRedeemers inputs redeemersTxIns = runExceptT do
  redeemersTxInsReindexed <- ExceptT $
    reindexSpentScriptRedeemers' inputs redeemersTxIns
  except <<< Right $
    map fst redeemersTxInsReindexed

reindexSpentScriptRedeemers'
  :: Array TransactionInput
  -> Array RedeemersTxIn
  -> QueryM (Either ReindexErrors (Array RedeemersTxIn))
reindexSpentScriptRedeemers' inputs redeemersTxIns = runExceptT do
  liftEither $ traverse (reindex inputs) redeemersTxIns
  where
  reindex
    :: Array TransactionInput
    -> RedeemersTxIn
    -> Either ReindexErrors RedeemersTxIn
  reindex ipts = case _ of
    red@(T.Redeemer red'@{ tag: Spend }) /\ Just txOutRef -> do
      index <- note (CannotGetTxOutRefIndexForRedeemer red)
        (fromInt <$> elemIndex txOutRef ipts)
      Right $ T.Redeemer red' { index = index } /\ Just txOutRef
    mintRed /\ txOutRef -> Right $ mintRed /\ txOutRef
