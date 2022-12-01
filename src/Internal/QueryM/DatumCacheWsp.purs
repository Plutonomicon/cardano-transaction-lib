module Ctl.Internal.QueryM.DatumCacheWsp
  ( DatumCacheMethod
      ( GetTxByHash
      )
  , GetTxByHashR(GetTxByHashR)
  , WspFault(WspFault)
  , faultToString
  , getTxByHashCall
  , JsonWspRequest
  , JsonWspResponse
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError
  , decodeAeson
  , getNestedAeson
  , stringifyAeson
  )
import Control.Alt ((<|>))
import Ctl.Internal.Base64 (Base64String)
import Ctl.Internal.QueryM.JsonWsp (JsonWspCall, mkCallType)
import Ctl.Internal.QueryM.UniqueId (ListenerId)
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype WspFault = WspFault Aeson

faultToString :: WspFault -> String
faultToString (WspFault j) = stringifyAeson j

type JsonWspRequest (a :: Type) =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , args :: a
  , mirror :: ListenerId
  }

type JsonWspResponse =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , result :: Maybe Aeson
  , fault :: Maybe WspFault
  , reflection :: ListenerId
  }

-- TODO
-- This should be changed to `GetTxByHashR Transaction` once we support `getTxById`
--
-- See https://github.com/Plutonomicon/cardano-transaction-lib/issues/30
newtype GetTxByHashR = GetTxByHashR (Maybe Base64String)

derive instance Newtype GetTxByHashR _
derive instance Generic GetTxByHashR _

instance Show GetTxByHashR where
  show = genericShow

instance DecodeAeson GetTxByHashR where
  decodeAeson r = GetTxByHashR <$>
    let
      txFound :: Either JsonDecodeError (Maybe Base64String)
      txFound =
        getNestedAeson r [ "TxFound", "value" ] >>= decodeAeson

      txNotFound :: Either JsonDecodeError (Maybe Base64String)
      txNotFound =
        Nothing <$ getNestedAeson r [ "TxNotFound" ]
    in
      txFound <|> txNotFound

-- TODO: delete
data DatumCacheMethod
  = GetTxByHash

derive instance Eq DatumCacheMethod

instance Show DatumCacheMethod where
  show = datumCacheMethodToString

datumCacheMethodToString :: DatumCacheMethod -> String
datumCacheMethodToString = case _ of
  GetTxByHash -> "GetTxByHash"

type TxHash = ByteArray

getTxByHashCall :: JsonWspCall TxHash GetTxByHashR
getTxByHashCall = mkDatumCacheCallType
  GetTxByHash
  ({ hash: _ } <<< byteArrayToHex)

-- convenience helper
mkDatumCacheCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonWspRequest a)
  => DatumCacheMethod
  -> (i -> a)
  -> JsonWspCall i o
mkDatumCacheCallType method args = mkCallType
  { "type": "jsonwsp/request"
  , version: "1.0"
  , servicename: "ogmios"
  }
  { methodname: datumCacheMethodToString method, args }

