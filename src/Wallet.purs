module Wallet
  ( Cip30Connection(..)
  , Cip30Wallet(..)
  , Wallet(..)
  , mkNamiWalletAff
  , mkGeroWalletAff
  , dummySign
  ) where

import Prelude

import Cardano.Types.Transaction
  ( Ed25519Signature(Ed25519Signature)
  , PublicKey(PublicKey)
  , Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  )
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Newtype (over, unwrap)
import Data.Tuple.Nested ((/\))
import Deserialization.FromBytes (fromBytesEffect)
import Deserialization.UnspentOutput as Deserialization.UnspentOuput
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Helpers ((<<>>))
import Serialization as Serialization
import Serialization.Address (Address, addressFromBytes)
import Types.ByteArray (byteArrayToHex)
import Types.CborBytes
  ( CborBytes
  , cborBytesToHex
  , rawBytesAsCborBytes
  )
import Types.RawBytes (RawBytes, hexToRawBytes)
import Untagged.Union (asOneOf)

type Cip30Wallet =
  { -- A reference to a connection with the wallet, i.e. `window.cardano.nami`
    connection :: Ref.Ref Cip30Connection
  -- Get the address associated with the wallet (Nami does not support
  -- multiple addresses)
  , getWalletAddress :: Cip30Connection -> Aff (Maybe Address)
  -- Get the collateral UTxO associated with the Nami wallet
  , getCollateral :: Cip30Connection -> Aff (Maybe TransactionUnspentOutput)
  -- Sign a transaction with the current wallet
  , signTx :: Cip30Connection -> Transaction -> Aff (Maybe Transaction)
  -- Sign a transaction with the current wallet
  , signTxBytes :: Cip30Connection -> CborBytes -> Aff (Maybe CborBytes)
  }

data Wallet = Nami Cip30Wallet | Gero Cip30Wallet

-------------------------------------------------------------------------------
-- Nami backend
-------------------------------------------------------------------------------
-- Record-of-functions for real or mocked Nami wallet, includes `Ref` to
-- connection (e.g., with `window.cardano.nami` as a `Cip30Connection`)

mkNamiWalletAff :: Aff Wallet
mkNamiWalletAff = do
  nami <- enable "nami"
  -- Ensure the Nami wallet has collateral set up
  whenM (isNothing <$> getCollateral nami)
    (liftEffect $ throw "Nami wallet missing collateral")
  connection <- liftEffect $ Ref.new nami
  pure $ Nami
    { connection
    , getWalletAddress
    , getCollateral
    , signTx
    , signTxBytes
    }

-- Attach a dummy vkey witness to a transaction. Helpful for when we need to
-- know the number of witnesses (e.g. fee calculation) but the wallet hasn't
-- signed (or cannot sign) yet
dummySign :: Transaction -> Transaction
dummySign tx@(Transaction { witnessSet: tws@(TransactionWitnessSet ws) }) =
  over Transaction _
    { witnessSet = over TransactionWitnessSet
        _
          { vkeys = ws.vkeys <<>> Just [ vk ]
          }
        tws
    }
    $ tx
  where
  vk :: Vkeywitness
  vk = Vkeywitness
    ( Vkey
        ( PublicKey
            "ed25519_pk1eamrnx3pph58yr5l4z2wghjpu2dt2f0rp0zq9qquqa39p52ct0xsudjp4e"
        )
        /\ Ed25519Signature
          "ed25519_sig1ynufn5umzl746ekpjtzt2rf58ep0wg6mxpgyezh8vx0e8jpgm3kuu3tgm453wlz4rq5yjtth0fnj0ltxctaue0dgc2hwmysr9jvhjzswt86uk"
    )

-------------------------------------------------------------------------------
-- Gero backend
-------------------------------------------------------------------------------

mkGeroWalletAff :: Aff Wallet
mkGeroWalletAff = do
  gero <- enable "gerowallet"
  -- Ensure the Gero wallet has collateral set up
  whenM (isNothing <$> getCollateral gero)
    (liftEffect $ throw "Gero wallet missing collateral")
  connection <- liftEffect $ Ref.new gero
  pure $ Gero
    { connection
    , getWalletAddress
    , getCollateral
    , signTx
    , signTxBytes
    }

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

enable :: String -> Aff Cip30Connection
enable = Promise.toAffE <<< _enable

txToHex :: Transaction -> Aff String
txToHex =
  liftEffect
    <<< map (byteArrayToHex <<< Serialization.toBytes <<< asOneOf)
    <<< Serialization.convertTransaction

getWalletAddress :: Cip30Connection -> Aff (Maybe Address)
getWalletAddress nami = fromHexString _getAddress nami <#>
  (_ >>= addressFromBytes <<< rawBytesAsCborBytes)

getCollateral :: Cip30Connection -> Aff (Maybe TransactionUnspentOutput)
getCollateral nami = fromMaybeHexString getCip30Collateral nami >>=
  case _ of
    Nothing -> pure Nothing
    Just bytes -> do
      liftEffect $
        Deserialization.UnspentOuput.convertUnspentOutput
          <$> fromBytesEffect (unwrap bytes)

signTx :: Cip30Connection -> Transaction -> Aff (Maybe Transaction)
signTx nami tx = do
  txHex <- txToHex tx
  fromHexString (_signTx txHex) nami >>= case _ of
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

signTxBytes :: Cip30Connection -> CborBytes -> Aff (Maybe CborBytes)
signTxBytes nami txBytes = do
  fromHexString (_signTx (cborBytesToHex txBytes)) nami >>= case _ of
    Nothing -> pure Nothing
    Just witBytes -> Just <$> liftEffect
      (_attachSignature txBytes (rawBytesAsCborBytes witBytes))

fromHexString act = map hexToRawBytes <<< Promise.toAffE <<< act

fromMaybeHexString act =
  map (flip bind hexToRawBytes) <<< Promise.toAffE <<< act

-------------------------------------------------------------------------------
-- FFI stuff
-------------------------------------------------------------------------------
foreign import data Cip30Connection :: Type

newtype NamiConnection = NamiConnection Cip30Connection

newtype GeroConnection = GeroConnection Cip30Connection

foreign import _enable :: String -> Effect (Promise Cip30Connection)

foreign import _getAddress :: Cip30Connection -> Effect (Promise String)

foreign import _getCollateral
  :: MaybeFfiHelper -> Cip30Connection -> Effect (Promise (Maybe String))

getCip30Collateral :: Cip30Connection -> Effect (Promise (Maybe String))
getCip30Collateral = _getCollateral maybeFfiHelper

foreign import _signTx
  :: String -- Hex-encoded cbor of tx
  -> Cip30Connection
  -> Effect (Promise String)

foreign import _attachSignature
  :: CborBytes -- CBOR bytes of tx
  -> CborBytes -- CBOR bytes of witness set
  -> Effect CborBytes
