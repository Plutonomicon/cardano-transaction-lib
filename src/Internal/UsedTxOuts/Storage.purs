-- | This module implements an interface for various shared local storages
-- | that CTL can use to permanently store `UsedTxOuts` contents.
-- | Shared storage is needed to ensure that multiple instances of CTL-based
-- | apps will not try to spend the same UTxOs concurrently.
-- |
-- | For example:
-- |
-- | - In the browser context, using `Ctl.Internal.UsedTxOuts.LocalStorage`
-- |   allows to make sure that two apps running in different tabs will not
-- |   race for UTxOs.
-- | - In the NodeJS context, it allows to ensure that multiple bots, if they
-- |   are using the same file for storage, will not race.
module Ctl.Internal.UsedTxOuts.Storage where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Ref as Ref

-- | An interface handle very similar to `LocalStorage` interface.
type Storage =
  { getItem :: String -> Effect (Maybe String)
  , setItem :: String -> String -> Effect Unit
  }

mkDisabledStorage :: Effect Storage
mkDisabledStorage = do
  ref <- Ref.new Map.empty
  pure
    { getItem: \key -> do
        Ref.read ref <#> Map.lookup key
    , setItem: \key value -> do
        Ref.modify_ (Map.insert key value) ref
    }
