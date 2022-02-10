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
import Data.Typelevel.Undefined (undefined)
import Deserialization.Address as Deserialization.Address
import Deserialization.UnspentOutput as Deserialization.UnspentOuput
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Serialization as Serialization
import Types.ByteArray (ByteArray, hexToByteArray)
import Types.Transaction (Address, Transaction)
import Types.TransactionUnspentOutput (TransactionUnspentOutput)

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
    }

  where
  enable :: Aff NamiConnection
  enable = Promise.toAffE $ _enableNami

  getWalletAddress :: NamiConnection -> Aff (Maybe Address)
  getWalletAddress nami = fromNamiHexString _getNamiAddress nami >>= case _ of
    Nothing -> pure Nothing
    Just bytes -> do
      liftEffect $
        Deserialization.Address.convertAddress
          <$> Serialization.newAddressFromBytes bytes

  getCollateral :: NamiConnection -> Aff (Maybe TransactionUnspentOutput)
  getCollateral nami = fromNamiHexString _getNamiCollateral nami >>= case _ of
    Nothing -> pure Nothing
    Just bytes -> do
      liftEffect $
        Deserialization.UnspentOuput.convertUnspentOutput
          <$> Serialization.newTransactionUnspentOutputFromBytes bytes

  signTx :: NamiConnection -> Transaction -> Aff (Maybe Transaction)
  signTx nami = undefined -- TODO

  fromNamiHexString
    :: (NamiConnection -> Effect (Promise String))
    -> NamiConnection
    -> Aff (Maybe ByteArray)
  fromNamiHexString act = map hexToByteArray <<< Promise.toAffE <<< act

-------------------------------------------------------------------------------
-- FFI stuff
-------------------------------------------------------------------------------
foreign import data NamiConnection :: Type

foreign import _enableNami :: Effect (Promise NamiConnection)

foreign import _getNamiAddress :: NamiConnection -> Effect (Promise String)

foreign import _getNamiCollateral :: NamiConnection -> Effect (Promise String)
