module Wallet
  ( NamiConnection
  , NamiWallet
  , Wallet(..)
  , mkNamiWalletAff
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(Just, Nothing))
import Deserialization.FromBytes (fromBytesEffect)
import Deserialization.UnspentOutput as Deserialization.UnspentOuput
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization as Serialization
import Serialization.Address (Address, addressFromBytes)
import Types.ByteArray (ByteArray, hexToByteArray, byteArrayToHex)
import Types.Transaction
  ( Transaction(Transaction)
  , TransactionHash(TransactionHash)
  , TransactionWitnessSet
  )
import Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Untagged.Union (asOneOf)

-- At the moment, we only support Nami's wallet. In the future we will expand
-- this with more constructors to represent out-of-browser wallets (e.g. WBE)
data Wallet = Nami NamiWallet

-------------------------------------------------------------------------------
-- Nami backend
-------------------------------------------------------------------------------
-- Record-of-functions for real or mocked Nami wallet, includes `Ref` to
-- connection (e.g. with `window.cardano.nami` as a `NamiConnection`)
type NamiWallet =
  { -- A reference to a connection with Nami, i.e. `window.cardano.nami`
    connection :: Ref.Ref NamiConnection
  -- Get the address associated with the wallet (Nami does not support
  -- multiple addresses)
  , getWalletAddress :: NamiConnection -> Aff (Maybe Address)
  -- Get the collateral UTxO associated with the Nami wallet
  , getCollateral :: NamiConnection -> Aff (Maybe TransactionUnspentOutput)
  -- Sign a transaction with the current wallet
  , signTx :: NamiConnection -> Transaction -> Aff (Maybe Transaction)
  -- Submit a (balanced) transaction
  , submitTx :: NamiConnection -> Transaction -> Aff (Maybe TransactionHash)
  -- Submit a (balanced) transaction encoded as CBORHex
  , submitTxBytes :: NamiConnection -> ByteArray -> Aff (Maybe TransactionHash)
  }

mkNamiWalletAff :: Aff Wallet
mkNamiWalletAff = do
  nami <- enable
  connection <- liftEffect $ Ref.new nami
  pure $ Nami
    { connection
    , getWalletAddress
    , getCollateral
    , signTx
    , submitTx
    , submitTxBytes
    }

  where
  enable :: Aff NamiConnection
  enable = Promise.toAffE $ _enableNami

  getWalletAddress :: NamiConnection -> Aff (Maybe Address)
  getWalletAddress nami = fromNamiHexString _getNamiAddress nami >>=
    (_ >>= addressFromBytes) >>> pure

  getCollateral :: NamiConnection -> Aff (Maybe TransactionUnspentOutput)
  getCollateral nami = fromNamiMaybeHexString getNamiCollateral nami >>= case _ of
    Nothing -> pure Nothing
    Just bytes -> do
      liftEffect $
        Deserialization.UnspentOuput.convertUnspentOutput
          <$> fromBytesEffect bytes

  signTx :: NamiConnection -> Transaction -> Aff (Maybe Transaction)
  signTx nami tx = do
    txHex <- txToHex tx
    fromNamiHexString (_signTxNami txHex) nami >>= case _ of
      Nothing -> pure Nothing
      Just bytes -> map (combineWitnessSet tx) <$> liftEffect
        ( Deserialization.WitnessSet.convertWitnessSet
            <$> fromBytesEffect bytes
        )
    where
    -- We have to combine the newly returned witness set with the existing one
    -- Otherwise, any datums, etc... won't be retained
    combineWitnessSet :: Transaction -> TransactionWitnessSet -> Transaction
    combineWitnessSet (Transaction tx'@{ witness_set: oldWits }) newWits =
      Transaction $ tx' { witness_set = oldWits <> newWits }

  submitTx :: NamiConnection -> Transaction -> Aff (Maybe TransactionHash)
  submitTx nami tx = submitTxBytes nami =<< txToBytes tx

  submitTxBytes :: NamiConnection -> ByteArray -> Aff (Maybe TransactionHash)
  submitTxBytes nami txBytes = do
    map TransactionHash <$> fromNamiHexString (_submitTxNami $ byteArrayToHex txBytes) nami

  txToHex :: Transaction -> Aff String
  txToHex =
    liftEffect
      <<< map (byteArrayToHex <<< Serialization.toBytes <<< asOneOf)
      <<< Serialization.convertTransaction

  txToBytes :: Transaction -> Aff ByteArray
  txToBytes =
    liftEffect
      <<< map (Serialization.toBytes <<< asOneOf)
      <<< Serialization.convertTransaction

  fromNamiHexString
    :: (NamiConnection -> Effect (Promise String))
    -> NamiConnection
    -> Aff (Maybe ByteArray)
  fromNamiHexString act = map hexToByteArray <<< Promise.toAffE <<< act

  fromNamiMaybeHexString
    :: (NamiConnection -> Effect (Promise (Maybe String)))
    -> NamiConnection
    -> Aff (Maybe ByteArray)
  fromNamiMaybeHexString act = map (flip bind hexToByteArray) <<< Promise.toAffE <<< act

-------------------------------------------------------------------------------
-- FFI stuff
-------------------------------------------------------------------------------
foreign import data NamiConnection :: Type

foreign import _enableNami :: Effect (Promise NamiConnection)

foreign import _getNamiAddress :: NamiConnection -> Effect (Promise String)

foreign import _getNamiCollateral :: MaybeFfiHelper -> NamiConnection -> Effect (Promise (Maybe String))

getNamiCollateral :: NamiConnection -> Effect (Promise (Maybe String))
getNamiCollateral = _getNamiCollateral maybeFfiHelper

foreign import _signTxNami
  :: String -- Hex-encoded cbor of tx
  -> NamiConnection
  -> Effect (Promise String)

foreign import _submitTxNami
  :: String -- Hex-encoded cbor of tx
  -> NamiConnection
  -> Effect (Promise String) -- Submitted tx hash
