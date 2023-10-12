module Contract.Hashing
  ( module X
  , transactionHash
  , publicKeyHash
  , auxiliaryDataHash
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction
  ( AuxiliaryData
  , AuxiliaryDataHash
  , PublicKey
  , Transaction
  , convertPubKey
  )
import Ctl.Internal.Hashing
  ( blake2b224Hash
  , blake2b224HashHex
  , blake2b256Hash
  , blake2b256HashHex
  , datumHash
  , md5HashHex
  , plutusScriptHash
  , scriptRefHash
  , sha256Hash
  , sha256HashHex
  , sha3_256Hash
  , sha3_256HashHex
  ) as X
import Ctl.Internal.Hashing (transactionHashImpl) as Internal
import Ctl.Internal.NativeScripts (nativeScriptHash) as X
import Ctl.Internal.Serialization (convertTransaction)
import Ctl.Internal.Serialization (publicKeyHashImpl) as Internal
import Ctl.Internal.Serialization.AuxiliaryData (hashAuxiliaryData)
import Ctl.Internal.Types.PubKeyHash (PubKeyHash)
import Ctl.Internal.Types.Transaction (TransactionHash)
import Data.Newtype (wrap)
import Effect (Effect)

transactionHash :: Transaction -> Effect TransactionHash
transactionHash tx = Internal.transactionHashImpl <$> convertTransaction tx

publicKeyHash :: PublicKey -> PubKeyHash
publicKeyHash pk = wrap $ Internal.publicKeyHashImpl $ convertPubKey pk

auxiliaryDataHash :: AuxiliaryData -> Effect AuxiliaryDataHash
auxiliaryDataHash = hashAuxiliaryData
