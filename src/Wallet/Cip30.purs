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
import Data.Maybe (Maybe(Just, Nothing), isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
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
import Serialization.Address (Address, addressFromBytes)
import Types.ByteArray (byteArrayToHex)
import Types.CborBytes (rawBytesAsCborBytes)
import Types.RawBytes (RawBytes, hexToRawBytes)
import Untagged.Union (asOneOf)

type Cip30Wallet =
  { -- A reference to a connection with the wallet, i.e. `window.cardano.nami`
    connection :: Cip30Connection
  -- Get the address associated with the wallet (Nami does not support
  -- multiple addresses)
  , getWalletAddresses :: Cip30Connection -> Aff (Maybe (Array Address))

  -- Get combination of all available UTxOs
  , getBalance :: Cip30Connection -> Aff (Maybe Value)
  -- Get the collateral UTxO associated with the Nami wallet
  , getCollateral ::
      Cip30Connection -> Aff (Maybe (Array TransactionUnspentOutput))
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
    , getWalletAddresses
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

getWalletAddresses :: Cip30Connection -> Aff (Maybe (Array Address))
getWalletAddresses conn = Promise.toAffE (_getAddresses conn) <#>
  (traverse ((addressFromBytes <<< rawBytesAsCborBytes) <=< hexToRawBytes))

-- | Get collateral using CIP-30 `getCollateral` method.
-- | Throws on `Promise` rejection by wallet, returns `Nothing` if no collateral
-- | is available.
getCollateral :: Cip30Connection -> Aff (Maybe (Array TransactionUnspentOutput))
getCollateral conn = do
  mbUtxoStrs <- toAffE $ getCip30Collateral conn
  let
    (mbUtxoBytes :: Maybe (Array RawBytes)) =
      join $ map (traverse hexToRawBytes) mbUtxoStrs
  -- de-serialize UTxOs
  liftEffect $ for mbUtxoBytes \collateralUtxos -> do
    for collateralUtxos \bytes -> do
      maybe (throw "Unable to convert UTxO") pure =<<
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

-------------------------------------------------------------------------------
-- FFI stuff
-------------------------------------------------------------------------------
foreign import data Cip30Connection :: Type

foreign import _getAddresses :: Cip30Connection -> Effect (Promise (Array
String))

foreign import _getCollateral
  :: MaybeFfiHelper
  -> Cip30Connection
  -> Effect (Promise (Maybe (Array String)))

getCip30Collateral :: Cip30Connection -> Effect (Promise (Maybe (Array String)))
getCip30Collateral = _getCollateral maybeFfiHelper

foreign import _signTx
  :: String -- Hex-encoded cbor of tx
  -> Cip30Connection
  -> Effect (Promise String)

foreign import _getBalance :: Cip30Connection -> Effect (Promise String)
