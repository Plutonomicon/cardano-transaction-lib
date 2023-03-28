-- | This module implements a data type that is conceptially equivalent to
-- | `Map TransactionInput Timestamp`. It is used to track UTxOs that are
-- | already consumed.
-- |
-- | See also: `Ctl.Internal.UsedTxOuts.Storage` module
module Ctl.Internal.UsedTxOuts where

import Prelude

import Aeson
  ( Finite
  , decodeAeson
  , encodeAeson
  , parseJsonStringToAeson
  , stringifyAeson
  )
import Ctl.Internal.Types.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  )
import Ctl.Internal.UsedTxOuts.Storage (Storage)
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.UInt (UInt)
import Effect (Effect)

type Timestamp = Finite Number

type UsedTxOuts = Map TransactionHash (Map UInt Timestamp)

lookup :: TransactionInput -> UsedTxOuts -> Maybe Timestamp
lookup (TransactionInput { transactionId, index }) usedTxOuts =
  Map.lookup transactionId usedTxOuts >>= Map.lookup index

insert :: Timestamp -> TransactionInput -> UsedTxOuts -> UsedTxOuts
insert timestamp (TransactionInput { transactionId, index }) usedTxOuts =
  let
    newItem = Map.singleton transactionId $ Map.singleton index timestamp
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
