-- | This module implements a data type that is conceptially equivalent to
-- | `Map TransactionInput Timestamp`. It is used to track UTxOs that are
-- | already consumed.
-- |
-- | See also: `Ctl.Internal.UsedTxOuts.Storage` module
module Ctl.Internal.UsedTxOuts
  ( unlockAllBy
  , UsedTxOuts
  , TxOutputLock
  , Timestamp
  , lockTransactionInputs
  , unlockTransactionInputs
  , get
  , lookup
  ) where

import Prelude

import Aeson
  ( Finite
  , decodeAeson
  , encodeAeson
  , finiteNumber
  , parseJsonStringToAeson
  , stringifyAeson
  )
import Control.MonadZero (guard)
import Ctl.Internal.Cardano.Types.Transaction (Transaction, _body, _inputs)
import Ctl.Internal.Contract.AppInstance (AppInstanceId)
import Ctl.Internal.Helpers (liftEither)
import Ctl.Internal.Types.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  )
import Ctl.Internal.UsedTxOuts.Storage (Storage)
import Data.DateTime.Instant (unInstant)
import Data.Either (hush, note)
import Data.Lens ((^.))
import Data.Map (Map, mapMaybe)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (all, foldr)
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Exception (error)
import Effect.Now (now)

type Timestamp = Finite Number

type TxOutputLock = { at :: Timestamp, by :: AppInstanceId }

type UsedTxOuts = Map TransactionHash (Map UInt TxOutputLock)

-- | Unlock all inputs locked by a given app instance.
unlockAllBy :: Storage -> AppInstanceId -> Effect Unit
unlockAllBy storage id = modify_ storage $ unlockAllByPure id

-- | Marks transaction's inputs as used.
-- | If some of them are used, returns `false` and does not mark the remaining.
lockTransactionInputs
  :: forall (m :: Type -> Type)
   . AppInstanceId
  -> Storage
  -> Transaction
  -> Effect Boolean
lockTransactionInputs id storage tx = do
  mbTimestamp <- now <#> unInstant >>> unwrap >>> finiteNumber
  timestamp <- liftEither $ note (error tsError) mbTimestamp
  usedTxOuts <- get storage
  let
    inputs = tx ^. _body <<< _inputs
    isUnlocked input = isNothing $ lookup input usedTxOuts
  if all isUnlocked inputs then do
    let usedTxOuts' = foldr (insert id timestamp) usedTxOuts inputs
    set storage usedTxOuts'
    pure true
  else
    pure false
  where
  tsError = "Impossible happened: unable to convert timestamp to finite number"

-- | Un-mark transaction's inputs as used, if they are marked.
-- TODO think this through
unlockTransactionInputs
  :: forall (m :: Type -> Type)
   . Storage
  -> Transaction
  -> Effect Unit
unlockTransactionInputs storage tx = do
  let
    inputs = tx ^. _body <<< _inputs
  usedTxOuts <- get storage
  let
    usedTxOuts' = foldr delete usedTxOuts inputs
  set storage usedTxOuts'

unlockAllByPure :: AppInstanceId -> UsedTxOuts -> UsedTxOuts
unlockAllByPure id = mapMaybe \locks ->
  let
    locks' =
      locks # mapMaybe \lock -> do
        guard (lock.by /= id)
        pure lock
  in
    do
      guard (not $ Map.isEmpty locks')
      pure $ locks'

lookup :: TransactionInput -> UsedTxOuts -> Maybe TxOutputLock
lookup (TransactionInput { transactionId, index }) usedTxOuts =
  Map.lookup transactionId usedTxOuts >>= Map.lookup index

insert
  :: AppInstanceId -> Timestamp -> TransactionInput -> UsedTxOuts -> UsedTxOuts
insert id timestamp (TransactionInput { transactionId, index }) usedTxOuts =
  let
    newItem = Map.singleton transactionId $ Map.singleton index
      { at: timestamp, by: id }
  in
    Map.unionWith (Map.unionWith max) usedTxOuts newItem

delete :: TransactionInput -> UsedTxOuts -> UsedTxOuts
delete (TransactionInput { transactionId, index }) =
  Map.update deleteInput transactionId
  where
  deleteInput mp =
    let
      updated = Map.delete index mp
    in
      if Map.isEmpty updated then Nothing else Just updated

serialize :: UsedTxOuts -> String
serialize = encodeAeson >>> stringifyAeson

parse :: String -> Maybe UsedTxOuts
parse = (parseJsonStringToAeson >=> decodeAeson) >>> hush

modify_ :: Storage -> (UsedTxOuts -> UsedTxOuts) -> Effect Unit
modify_ storage f = get storage >>= f >>> set storage

get :: Storage -> Effect UsedTxOuts
get { getItem } =
  getItem ctlStorageKey <#> flip bind parse >>> fromMaybe Map.empty

set :: Storage -> UsedTxOuts -> Effect Unit
set { setItem } usedTxOuts = setItem ctlStorageKey (serialize usedTxOuts)

ctlStorageKey :: String
ctlStorageKey = "CARDANO_TRANSACTION_LIB_LOCKED_UTXOS"
