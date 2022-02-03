module Deserialization
  ( convertAddress
  ) where

import Control.Alt ((<|>))
import Data.Maybe (Maybe (Just, Nothing))
import Prelude
import Serialization.Types (Address, BaseAddress, Ed25519KeyHash, ScriptHash, StakeCredential)
import Types.Transaction as T
import Untagged.Union (type (|+|), asOneOf)
import Data.UInt as UInt
import Serialization (toBytes)

foreign import _baseAddressFromAddress :: (forall a. Maybe a) -> (forall a. a -> Maybe a) -> Address -> Maybe BaseAddress
foreign import addressNetworkId :: Address -> Int
foreign import baseAddressStakeCredential :: BaseAddress -> StakeCredential
foreign import baseAddressPaymentCredential :: BaseAddress -> StakeCredential
foreign import _stakeCredentialToScriptHash :: (forall a. Maybe a) -> (forall a. a -> Maybe a) -> StakeCredential -> Maybe ScriptHash
foreign import _stakeCredentialToKeyHash :: (forall a. Maybe a) -> (forall a. a -> Maybe a) -> StakeCredential -> Maybe Ed25519KeyHash

stakeCredentialToKeyHash :: StakeCredential -> Maybe Ed25519KeyHash
stakeCredentialToKeyHash = _stakeCredentialToKeyHash Nothing Just

stakeCredentialToScriptHash :: StakeCredential -> Maybe ScriptHash
stakeCredentialToScriptHash = _stakeCredentialToScriptHash Nothing Just

baseAddressFromAddress :: Address -> Maybe BaseAddress
baseAddressFromAddress = _baseAddressFromAddress Nothing Just

foreign import toBech32
  :: ( Ed25519KeyHash
         |+| ScriptHash
     -- Add more as needed.
     )
  -> T.Bech32

convertAddress :: Address -> Maybe T.Address
convertAddress address = do
  baseAddress <- baseAddressFromAddress address
  stake <- convertStakeCredential $ baseAddressStakeCredential baseAddress
  payment <- convertPaymentCredential $ baseAddressPaymentCredential baseAddress
  pure $ T.Address
    { "AddrType": T.BaseAddress
        { network: UInt.fromInt $ addressNetworkId address
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
convertScriptHash = T.ScriptHash <<< toBech32 <<< asOneOf

convertPaymentCredential :: StakeCredential -> Maybe T.PaymentCredential
convertPaymentCredential cred =
  (T.PaymentCredentialKey <<< convertEd25519KeyHash <$> stakeCredentialToKeyHash cred) <|>
    (T.PaymentCredentialScript <<< convertScriptHash <$> stakeCredentialToScriptHash cred)
