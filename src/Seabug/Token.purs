module Seabug.Token
  ( mkTokenName
  , policy
  )
  where

import Contract.Prelude
import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Scripts (MintingPolicy, applyArgsM)
import Contract.Value (mkTokenName) as Value
import Contract.Value (TokenName)
import Seabug.Types (NftCollection(NftCollection), NftId, hash)

-- rev: 2c9ce295ccef4af3f3cb785982dfe554f8781541
mkTokenName :: NftId -> Maybe TokenName
mkTokenName = Value.mkTokenName <=< hash

-- rev: 2c9ce295ccef4af3f3cb785982dfe554f8781541
-- Apply arguments to an unapplied `MintingPolicy` to give a `MintingPolicy`.
-- It's possible this is given as JSON since we don't have
-- `mkMintingPolicyScript`. I'm not convinced this type signature actually makes
-- sense.
policy
  :: NftCollection
  -> MintingPolicy
  -> Contract (Maybe MintingPolicy)
policy
  ( NftCollection
      { collectionNftCs -- CurrencySymbol
      , lockLockup -- BigInt
      , lockLockupEnd -- Slot
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
      , toData lockLockup
      , toData lockLockupEnd
      , toData lockingScript
      , toData author
      , toData authorShare
      , toData daoScript
      , toData daoShare
      ]
  in
    applyArgsM mph pd