module Wallet.Key
  ( KeyWallet'
  , mkKeyWallet'
  ) where

import Prelude

import Cardano.Types.Transaction
  ( Transaction(Transaction)
  , Utxo
  , _vkeys
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Cardano.Types.Value (NonAdaAsset(NonAdaAsset), Value(Value), mkCoin)
import Contract.Prelude (class Newtype)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Lens (set)
import Data.List (all)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Ord.Min (Min(Min))
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Serialization (publicKeyFromPrivateKey, publicKeyHash)
import Serialization as Serialization
import Serialization.Address
  ( Address
  , NetworkId
  , enterpriseAddress
  , enterpriseAddressToAddress
  , keyHashCredential
  )
import Serialization.Types (PrivateKey)

-------------------------------------------------------------------------------
-- Key backend
-------------------------------------------------------------------------------
type KeyWallet' =
  { address :: NetworkId -> Aff Address
  , selectCollateral :: Utxo -> Maybe TransactionUnspentOutput
  , signTx :: Transaction -> Aff Transaction
  }

mkKeyWallet' :: PrivateKey -> KeyWallet'
mkKeyWallet' key =
  { address
  , selectCollateral
  , signTx
  }
  where
  address :: NetworkId -> Aff Address
  address network = publicKeyFromPrivateKey key
    # liftEffect
    <#> publicKeyHash
      >>> keyHashCredential
      >>> { network, paymentCred: _ }
      >>> enterpriseAddress
      >>> enterpriseAddressToAddress

  selectCollateral :: Utxo -> Maybe TransactionUnspentOutput
  selectCollateral utxos = unwrap <<< unwrap <$> flip
    foldMapWithIndex
    utxos
    \input output ->
      let
        txuo = AdaOut $ TransactionUnspentOutput { input, output }
        Value ada (NonAdaAsset naa) = _value txuo
        onlyAda = all (all ((==) zero)) naa
        bigAda = ada >= mkCoin 5_000_000
      in
        if onlyAda && bigAda then Just $ Min txuo
        else Nothing

  signTx :: Transaction -> Aff Transaction
  signTx (Transaction tx) = liftEffect do
    txBody <- Serialization.convertTxBody tx.body
    hash <- Serialization.hashTransaction txBody
    wit <- Deserialization.WitnessSet.convertVkeyWitness <$>
      Serialization.makeVkeywitness hash key
    let witnessSet' = set _vkeys (pure $ pure wit) mempty
    pure $ Transaction $ tx { witnessSet = witnessSet' <> tx.witnessSet }

_value :: AdaOut -> Value
_value
  (AdaOut (TransactionUnspentOutput { output: TransactionOutput { amount } })) =
  amount

-- A wrapper around a UTxO, ordered by ada value
newtype AdaOut = AdaOut TransactionUnspentOutput

derive instance Newtype AdaOut _

instance Eq AdaOut where
  eq a b
    | Value a' _ <- _value a
    , Value b' _ <- _value b = eq a' b'

instance Ord AdaOut where
  compare a b
    | Value a' _ <- _value a
    , Value b' _ <- _value b = compare a' b'
