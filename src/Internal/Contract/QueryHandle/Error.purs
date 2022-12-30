module Ctl.Internal.Contract.QueryHandle.Error
  ( GetTxMetadataError
      ( GetTxMetadataTxNotFoundError
      , GetTxMetadataMetadataEmptyOrMissingError
      , GetTxMetadataClientError
      )
  ) where

import Prelude

import Ctl.Internal.Service.Error (ClientError)

-- Abstracts over the differences between Kupo and Blockfrost
data GetTxMetadataError
  = GetTxMetadataTxNotFoundError
  | GetTxMetadataMetadataEmptyOrMissingError
  | GetTxMetadataClientError ClientError

instance Show GetTxMetadataError where
  show = case _ of
    GetTxMetadataTxNotFoundError ->
      "GetTxMetadataTxNotFoundError"
    GetTxMetadataMetadataEmptyOrMissingError ->
      "GetTxMetadataMetadataEmptyOrMissingError"
    GetTxMetadataClientError error ->
      "(GetTxMetadataClientError " <> show error <> ")"
