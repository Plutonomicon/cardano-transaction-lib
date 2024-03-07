module Ctl.Internal.Types.MintingPolicyHash
  ( MintingPolicyHash(MintingPolicyHash)
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson)
import Cardano.FromData (class FromData)
import Cardano.FromMetadata (class FromMetadata)
import Cardano.ToData (class ToData)
import Cardano.ToMetadata (class ToMetadata)
import Cardano.Types.ScriptHash (ScriptHash)
import Ctl.Internal.Helpers (decodeTaggedNewtype)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype MintingPolicyHash = MintingPolicyHash ScriptHash

derive instance Generic MintingPolicyHash _
derive instance Newtype MintingPolicyHash _
derive newtype instance Eq MintingPolicyHash
derive newtype instance Ord MintingPolicyHash
derive newtype instance FromData MintingPolicyHash
derive newtype instance ToData MintingPolicyHash
derive newtype instance FromMetadata MintingPolicyHash
derive newtype instance ToMetadata MintingPolicyHash

instance DecodeAeson MintingPolicyHash where
  decodeAeson = decodeTaggedNewtype "getMintingPolicyHash" MintingPolicyHash

instance EncodeAeson MintingPolicyHash where
  encodeAeson (MintingPolicyHash hash) =
    encodeAeson { "getMintingPolicyHash": hash }

instance Show MintingPolicyHash where
  show = genericShow
