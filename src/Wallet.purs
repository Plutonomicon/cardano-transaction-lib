module Wallet
  ( NamiConnection
  , NamiWallet
  , Wallet(..)
  , mkNamiWalletAff
  , dummySign
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Newtype (over, wrap, unwrap)
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
  , hexToCborBytes
  , cborBytesToHex
  , rawBytesAsCborBytes
  )
import Types.RawBytes (RawBytes, hexToRawBytes, rawBytesToHex)
import Types.Transaction
  ( Ed25519Signature(Ed25519Signature)
  , PublicKey(PublicKey)
  , Transaction(Transaction)
  , TransactionHash(TransactionHash)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
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
  -- Sign a transaction with the current wallet
  , signTxBytes :: NamiConnection -> CborBytes -> Aff (Maybe CborBytes)
  -- Submit a (balanced) transaction
  , submitTx :: NamiConnection -> Transaction -> Aff (Maybe TransactionHash)
  }

mkNamiWalletAff :: Aff Wallet
mkNamiWalletAff = do
  nami <- enable
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
    , submitTx
    }

  where
  enable :: Aff NamiConnection
  enable = Promise.toAffE $ _enableNami

  getWalletAddress :: NamiConnection -> Aff (Maybe Address)
  getWalletAddress nami = fromNamiHexString _getNamiAddress nami <#>
    (_ >>= addressFromBytes <<< rawBytesAsCborBytes)

  getCollateral :: NamiConnection -> Aff (Maybe TransactionUnspentOutput)
  getCollateral nami = fromNamiMaybeHexString getNamiCollateral nami >>=
    case _ of
      Nothing -> pure Nothing
      Just bytes -> do
        liftEffect $
          Deserialization.UnspentOuput.convertUnspentOutput
            <$> fromBytesEffect (unwrap bytes)

  signTx :: NamiConnection -> Transaction -> Aff (Maybe Transaction)
  signTx nami tx = do
    txHex <- txToHex tx
    fromNamiHexString (_signTxNami txHex) nami >>= case _ of
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

  signTxBytes :: NamiConnection -> CborBytes -> Aff (Maybe CborBytes)
  signTxBytes nami txBytes = do
    fromNamiHexString (_signTxNami (cborBytesToHex txBytes)) nami >>= case _ of
      Nothing -> pure Nothing
      Just witBytes -> Just <$> liftEffect
        (_attachSignature txBytes (rawBytesAsCborBytes witBytes))

  submitTx :: NamiConnection -> Transaction -> Aff (Maybe TransactionHash)
  submitTx nami tx = do
    txHex <- txToHex tx
    map (TransactionHash <<< unwrap) <$> fromNamiHexString (_submitTxNami txHex)
      nami

  txToHex :: Transaction -> Aff String
  txToHex =
    liftEffect
      <<< map (byteArrayToHex <<< Serialization.toBytes <<< asOneOf)
      <<< Serialization.convertTransaction

  fromNamiHexString
    :: (NamiConnection -> Effect (Promise String))
    -> NamiConnection
    -> Aff (Maybe RawBytes)
  fromNamiHexString act = map hexToRawBytes <<< Promise.toAffE <<< act

  fromNamiMaybeHexString
    :: (NamiConnection -> Effect (Promise (Maybe String)))
    -> NamiConnection
    -> Aff (Maybe RawBytes)
  fromNamiMaybeHexString act =
    map (flip bind hexToRawBytes) <<< Promise.toAffE <<< act

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
-- FFI stuff
-------------------------------------------------------------------------------
foreign import data NamiConnection :: Type

foreign import _enableNami :: Effect (Promise NamiConnection)

foreign import _getNamiAddress :: NamiConnection -> Effect (Promise String)

foreign import _getNamiCollateral
  :: MaybeFfiHelper -> NamiConnection -> Effect (Promise (Maybe String))

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

foreign import _attachSignature
  :: CborBytes -- CBOR bytes of tx
  -> CborBytes -- CBOR bytes of witness set
  -> Effect (CborBytes)
