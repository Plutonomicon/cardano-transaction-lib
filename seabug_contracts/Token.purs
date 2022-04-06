module Seabug.Token
  ( mkTokenName
  , policy
  , unappliedMintingPolicy
  ) where

import Contract.Prelude
import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Scripts (MintingPolicy, applyArgsM)
import Contract.Value (mkTokenName) as Value
import Contract.Value (TokenName)
import Data.Argonaut (Json, JsonDecodeError)
import Seabug.Helpers (jsonReader)
import Seabug.Types (NftCollection(NftCollection), NftId, hash)

-- rev: 2c9ce295ccef4af3f3cb785982dfe554f8781541
mkTokenName :: NftId -> Contract (Maybe TokenName)
mkTokenName nftId = hash nftId <#> maybe Nothing Value.mkTokenName

-- rev: 2c9ce295ccef4af3f3cb785982dfe554f8781541
-- Apply arguments to an unapplied `MintingPolicy` to give a `MintingPolicy`.
-- It's possible this is given as JSON since we don't have
-- `mkMintingPolicyScript`.
policy
  :: NftCollection
  -> MintingPolicy
  -> Contract (Maybe MintingPolicy)
policy
  ( NftCollection
      { collectionNftCs -- CurrencySymbol
      , lockingScript -- ValidatorHash
      , author -- PaymentPubKeyHash
      , authorShare -- Natural
      , daoScript -- ValidatorHash
      , daoShare -- Natural
      }
  )
  mph =
  let
    pd =
      [ toData collectionNftCs
      , toData lockingScript
      , toData author
      , toData authorShare
      , toData daoScript
      , toData daoShare
      ]
  in
    applyArgsM mph pd

-- This is read in locally as a minting policy with unapplied arguments. We
-- may prefer to change this to a fully appllied MintingPolicy.
unappliedMintingPolicy :: Either JsonDecodeError MintingPolicy
unappliedMintingPolicy = jsonReader "mintingPolicy" _unappliedMintingPolicy

foreign import _unappliedMintingPolicy :: Json
