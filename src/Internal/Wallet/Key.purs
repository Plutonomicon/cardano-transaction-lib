module Ctl.Internal.Wallet.Key
  ( KeyWallet(KeyWallet)
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeysToAddress
  , privateKeysToKeyWallet
  , keyWalletPrivatePaymentKey
  , keyWalletPrivateStakeKey
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Contract.Prelude (class Newtype)
import Ctl.Internal.BalanceTx.Collateral.Select (selectCollateral) as Collateral
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction(Transaction)
  , TransactionWitnessSet
  , UtxoMap
  , _vkeys
  )
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import Ctl.Internal.Deserialization.Keys
  ( privateKeyFromBech32
  , privateKeyToBech32
  )
import Ctl.Internal.Deserialization.WitnessSet as Deserialization.WitnessSet
import Ctl.Internal.Serialization
  ( publicKeyHashImpl
  )
import Ctl.Internal.Serialization as Serialization
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId
  , baseAddress
  , baseAddressToAddress
  , enterpriseAddress
  , enterpriseAddressToAddress
  , keyHashCredential
  )
import Ctl.Internal.Serialization.Keys (publicKeyFromPrivateKey)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Types.ProtocolParameters (CoinsPerUtxoUnit)
import Ctl.Internal.Types.RawBytes (RawBytes)
import Ctl.Internal.Wallet.Cip30 (DataSignature)
import Ctl.Internal.Wallet.Cip30.SignData (signData) as Cip30SignData
import Data.Array (fromFoldable)
import Data.Either (note)
import Data.Foldable (fold)
import Data.Lens (set)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

-------------------------------------------------------------------------------
-- Key backend
-------------------------------------------------------------------------------
newtype KeyWallet = KeyWallet
  { address :: NetworkId -> Address
  , selectCollateral ::
      CoinsPerUtxoUnit
      -> Int
      -> UtxoMap
      -> Effect (Maybe (Array TransactionUnspentOutput))
  , signTx :: Transaction -> Aff TransactionWitnessSet
  , signData :: NetworkId -> RawBytes -> Aff DataSignature
  , paymentKey :: PrivatePaymentKey
  , stakeKey :: Maybe PrivateStakeKey
  }

derive instance Newtype KeyWallet _

newtype PrivatePaymentKey = PrivatePaymentKey PrivateKey

derive instance Newtype PrivatePaymentKey _

instance Show PrivatePaymentKey where
  show _ = "(PrivatePaymentKey <hidden>)"

instance EncodeAeson PrivatePaymentKey where
  encodeAeson (PrivatePaymentKey pk) = encodeAeson (privateKeyToBech32 pk)

instance DecodeAeson PrivatePaymentKey where
  decodeAeson aeson =
    decodeAeson aeson >>=
      note (TypeMismatch "PrivateKey")
        <<< map PrivatePaymentKey
        <<< privateKeyFromBech32

newtype PrivateStakeKey = PrivateStakeKey PrivateKey

derive instance Newtype PrivateStakeKey _

instance Show PrivateStakeKey where
  show _ = "(PrivateStakeKey <hidden>)"

instance EncodeAeson PrivateStakeKey where
  encodeAeson (PrivateStakeKey pk) = encodeAeson (privateKeyToBech32 pk)

instance DecodeAeson PrivateStakeKey where
  decodeAeson aeson =
    decodeAeson aeson >>=
      note (TypeMismatch "PrivateKey")
        <<< map PrivateStakeKey
        <<< privateKeyFromBech32

keyWalletPrivatePaymentKey :: KeyWallet -> PrivatePaymentKey
keyWalletPrivatePaymentKey = unwrap >>> _.paymentKey

keyWalletPrivateStakeKey :: KeyWallet -> Maybe PrivateStakeKey
keyWalletPrivateStakeKey = unwrap >>> _.stakeKey

privateKeysToAddress
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> NetworkId -> Address
privateKeysToAddress payKey mbStakeKey network = do
  let pubPayKey = publicKeyFromPrivateKey (unwrap payKey)
  case mbStakeKey of
    Just stakeKey ->
      let
        pubStakeKey = publicKeyFromPrivateKey (unwrap stakeKey)
      in
        baseAddressToAddress $
          baseAddress
            { network
            , paymentCred: keyHashCredential $ publicKeyHashImpl $ pubPayKey
            , delegationCred: keyHashCredential $ publicKeyHashImpl $
                pubStakeKey
            }

    Nothing -> pubPayKey # publicKeyHashImpl
      >>> keyHashCredential
      >>> { network, paymentCred: _ }
      >>> enterpriseAddress
      >>> enterpriseAddressToAddress

privateKeysToKeyWallet
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> KeyWallet
privateKeysToKeyWallet payKey mbStakeKey =
  KeyWallet
    { address
    , selectCollateral
    , signTx
    , signData
    , paymentKey: payKey
    , stakeKey: mbStakeKey
    }
  where
  address :: NetworkId -> Address
  address = privateKeysToAddress payKey mbStakeKey

  selectCollateral
    :: CoinsPerUtxoUnit
    -> Int
    -> UtxoMap
    -> Effect (Maybe (Array TransactionUnspentOutput))
  selectCollateral coinsPerUtxoByte maxCollateralInputs utxos = map fromFoldable
    <$> Collateral.selectCollateral coinsPerUtxoByte maxCollateralInputs utxos

  signTx :: Transaction -> Aff TransactionWitnessSet
  signTx (Transaction tx) = liftEffect do
    txBody <- Serialization.convertTxBody tx.body
    hash <- Serialization.hashTransaction txBody
    payWitness <- Deserialization.WitnessSet.convertVkeyWitness <$>
      Serialization.makeVkeywitness hash (unwrap payKey)
    mbStakeWitness <- for mbStakeKey \stakeKey -> do
      Deserialization.WitnessSet.convertVkeyWitness <$>
        Serialization.makeVkeywitness hash (unwrap stakeKey)
    let
      witnessSet' = set _vkeys
        (pure $ [ payWitness ] <> fold (pure <$> mbStakeWitness))
        mempty
    pure witnessSet'

  signData :: NetworkId -> RawBytes -> Aff DataSignature
  signData networkId payload = do
    liftEffect $ Cip30SignData.signData (unwrap payKey) (address networkId)
      payload
