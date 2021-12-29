module Types.JsonWsp where

import Prelude

type Address = String
-- these types are described in: https://ogmios.dev/getting-started/basics/ 
type JsonWspRequest a =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , args :: a
  , mirror :: Mirror
  }

type Mirror = { step :: String }

mkJsonWspQuery :: forall a. a -> JsonWspRequest a 
mkJsonWspQuery a = 
  { type : "jsonwsp/request",
    version: "1.0",
    servicename: "ogmios",
    methodname: "Query",
    args: a,
    mirror: { step: "INIT" }
  }

type UtxoQueryParams = { utxo :: Array Address }
type QueryArgs a = { query :: a }
type UtxoQueryBody = JsonWspRequest (QueryArgs UtxoQueryParams)
