module Wallet.Cip30
  ( Cip30Connection
  , Cip30Wallet
  , mkCip30WalletAff
  ) where

import Prelude

import Cardano.Types.Transaction
  ( Transaction(Transaction)
  , TransactionWitnessSet
  )
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.Value (Value)
import Control.Promise (Promise, toAffE)
import Control.Promise as Promise
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Newtype (unwrap)
import Deserialization.FromBytes (fromBytes, fromBytesEffect)
import Deserialization.UnspentOutput (convertValue)
import Deserialization.UnspentOutput as Deserialization.UnspentOuput
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization as Serialization
import Serialization.Address
  ( Address
  , addressFromBytes
  )
import Types.ByteArray (byteArrayToHex)
import Types.CborBytes (rawBytesAsCborBytes)
import Types.RawBytes (RawBytes, hexToRawBytes)
import Untagged.Union (asOneOf)

type Cip30Wallet =
  { -- A reference to a connection with the wallet, i.e. `window.cardano.nami`
    connection :: Cip30Connection
  -- Get the address associated with the wallet (Nami does not support
  -- multiple addresses)
  , getWalletAddress :: Cip30Connection -> Aff (Maybe Address)
  -- Get combination of all available UTxOs
  , getBalance :: Cip30Connection -> Aff (Maybe Value)
  -- Get the collateral UTxO associated with the Nami wallet
  , getCollateral :: Cip30Connection -> Aff (Maybe TransactionUnspentOutput)
  -- Sign a transaction with the given wallet
  , signTx :: Cip30Connection -> Transaction -> Aff (Maybe Transaction)
  }

mkCip30WalletAff
  :: String
  -- ^ Name of the wallet for error messages
  -> Effect (Promise Cip30Connection)
  -- ^ A function to get wallet connection
  -> Aff Cip30Wallet
mkCip30WalletAff walletName enableWallet = do
  wallet <- toAffE enableWallet
  -- Ensure the Nami wallet has collateral set up
  whenM (isNothing <$> getCollateral wallet) do
    liftEffect $ throw $ walletName <> " wallet missing collateral"
  pure
    { connection: wallet
    , getWalletAddress
    , getCollateral
    , signTx
    , getBalance
    }

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

txToHex :: Transaction -> Aff String
txToHex =
  liftEffect
    <<< map (byteArrayToHex <<< Serialization.toBytes <<< asOneOf)
    <<< Serialization.convertTransaction

getWalletAddress :: Cip30Connection -> Aff (Maybe Address)
getWalletAddress conn = fromHexString _getAddress conn <#>
  (_ >>= addressFromBytes <<< rawBytesAsCborBytes)

getCollateral :: Cip30Connection -> Aff (Maybe TransactionUnspentOutput)
getCollateral conn = fromMaybeHexString getCip30Collateral conn >>=
  case _ of
    Nothing -> pure Nothing
    Just bytes -> do
      liftEffect $
        Deserialization.UnspentOuput.convertUnspentOutput
          <$> fromBytesEffect (unwrap bytes)

signTx :: Cip30Connection -> Transaction -> Aff (Maybe Transaction)
signTx conn tx = do
  txHex <- txToHex tx
  fromHexString (_signTx txHex) conn >>= case _ of
    Nothing -> pure Nothing
    Just bytes -> map (combineWitnessSet tx) <$> liftEffect
      ( Deserialization.WitnessSet.convertWitnessSet
          <$> fromBytesEffect (unwrap bytes)
      )
  where
  -- We have to combine the newly returned witness set with the existing one
  -- Otherwise, any datums, etc... won't be retained
  combineWitnessSet :: Transaction -> TransactionWitnessSet -> Transaction
  combineWitnessSet (Transaction tx'@{ witnessSet: oldWits }) newWits =
    Transaction $ tx' { witnessSet = oldWits <> newWits }

getBalance :: Cip30Connection -> Aff (Maybe Value)
getBalance wallet = do
  fromHexString _getBalance wallet <#> \mbBytes -> do
    bytes <- mbBytes
    fromBytes (unwrap bytes) >>= convertValue

fromHexString
  :: (Cip30Connection -> Effect (Promise String))
  -> Cip30Connection
  -> Aff (Maybe RawBytes)
fromHexString act = map hexToRawBytes <<< Promise.toAffE <<< act

fromMaybeHexString
  :: (Cip30Connection -> Effect (Promise (Maybe String)))
  -> Cip30Connection
  -> Aff (Maybe RawBytes)
fromMaybeHexString act =
  map (flip bind hexToRawBytes) <<< Promise.toAffE <<< act

-------------------------------------------------------------------------------
-- FFI stuff
-------------------------------------------------------------------------------
foreign import data Cip30Connection :: Type

foreign import _getAddress :: Cip30Connection -> Effect (Promise String)

foreign import _getCollateral
  :: MaybeFfiHelper -> Cip30Connection -> Effect (Promise (Maybe String))

getCip30Collateral :: Cip30Connection -> Effect (Promise (Maybe String))
getCip30Collateral = _getCollateral maybeFfiHelper

foreign import _signTx
  :: String -- Hex-encoded cbor of tx
  -> Cip30Connection
  -> Effect (Promise String)

foreign import _getBalance :: Cip30Connection -> Effect (Promise String)
