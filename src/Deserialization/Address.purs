module Deserialization.Address
  ( convertAddress
  , convertEd25519KeyHash
  ) where

import Control.Alt ((<|>))
import Data.Maybe (Maybe)
import Data.UInt as UInt
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Prelude
import Serialization (toBytes)
import Serialization.Types (Address, BaseAddress, Ed25519KeyHash, ScriptHash, StakeCredential)
import Types.Transaction as T
import Untagged.Union (asOneOf)

foreign import _baseAddressFromAddress :: MaybeFfiHelper -> Address -> Maybe BaseAddress
foreign import addressNetworkId :: Address -> Int
foreign import baseAddressStakeCredential :: BaseAddress -> StakeCredential
foreign import baseAddressPaymentCredential :: BaseAddress -> StakeCredential
foreign import _stakeCredentialToScriptHash :: MaybeFfiHelper -> StakeCredential -> Maybe ScriptHash
foreign import _stakeCredentialToKeyHash :: MaybeFfiHelper -> StakeCredential -> Maybe Ed25519KeyHash

stakeCredentialToKeyHash :: StakeCredential -> Maybe Ed25519KeyHash
stakeCredentialToKeyHash = _stakeCredentialToKeyHash maybeFfiHelper

stakeCredentialToScriptHash :: StakeCredential -> Maybe ScriptHash
stakeCredentialToScriptHash = _stakeCredentialToScriptHash maybeFfiHelper

baseAddressFromAddress :: Address -> Maybe BaseAddress
baseAddressFromAddress = _baseAddressFromAddress maybeFfiHelper

convertAddress :: Address -> Maybe T.Address
convertAddress address = do
  baseAddress <- baseAddressFromAddress address
  stake <- convertStakeCredential $ baseAddressStakeCredential baseAddress
  payment <- convertPaymentCredential $ baseAddressPaymentCredential baseAddress
  network <- UInt.fromInt' $ addressNetworkId address
  pure $ T.Address
    { "AddrType": T.BaseAddress
        { network
        , stake
        , payment
        }
    }

convertStakeCredential :: StakeCredential -> Maybe T.StakeCredential
convertStakeCredential cred =
  (T.StakeCredentialKey <<< convertEd25519KeyHash <$> stakeCredentialToKeyHash cred) <|>
    (T.StakeCredentialScript <<< convertScriptHash <$> stakeCredentialToScriptHash cred)

convertEd25519KeyHash :: Ed25519KeyHash -> T.Ed25519KeyHash
convertEd25519KeyHash = T.Ed25519KeyHash <<< toBytes <<< asOneOf

convertScriptHash :: ScriptHash -> T.ScriptHash
convertScriptHash = T.ScriptHash <<< toBytes <<< asOneOf

convertPaymentCredential :: StakeCredential -> Maybe T.PaymentCredential
convertPaymentCredential cred =
  (T.PaymentCredentialKey <<< convertEd25519KeyHash <$> stakeCredentialToKeyHash cred) <|>
    (T.PaymentCredentialScript <<< convertScriptHash <$> stakeCredentialToScriptHash cred)
