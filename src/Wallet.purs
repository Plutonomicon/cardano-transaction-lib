module Wallet
  ( NamiConnection
  , GeroConnection
  , NamiWallet
  , GeroWallet
  , Cip30Wallet(..)
  , Wallet(..)
  , mkNamiWalletAff
  , mkGeroWalletAff
  , dummySign
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Newtype (over)
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
import Types.ByteArray (ByteArray, hexToByteArray, byteArrayToHex)
import Types.Transaction
  ( Ed25519Signature(Ed25519Signature)
  , PublicKey(PublicKey)
  , Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  )
import Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Untagged.Union (asOneOf)

type Cip30Wallet a =
  { -- A reference to a connection with the wallet, i.e. `window.cardano.nami`
    connection :: Ref.Ref a
  -- Get the address associated with the wallet (Nami does not support
  -- multiple addresses)
  , getWalletAddress :: a -> Aff (Maybe Address)
  -- Get the collateral UTxO associated with the Nami wallet
  , getCollateral :: a -> Aff (Maybe TransactionUnspentOutput)
  -- Sign a transaction with the current wallet
  , signTx :: a -> Transaction -> Aff (Maybe Transaction)
  -- Sign a transaction with the current wallet
  , signTxBytes :: a -> ByteArray -> Aff (Maybe ByteArray)
  }

data Wallet = Nami NamiWallet | Gero GeroWallet

-------------------------------------------------------------------------------
-- Nami backend
-------------------------------------------------------------------------------
-- Record-of-functions for real or mocked Nami wallet, includes `Ref` to
-- connection (e.g. with `window.cardano.nami` as a `NamiConnection`)
type NamiWallet = Cip30Wallet NamiConnection

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
type GeroWallet = Cip30Wallet GeroConnection

mkGeroWalletAff :: Aff Wallet
mkGeroWalletAff = do
  gero <- enable
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
-- Generics stuff
-------------------------------------------------------------------------------

txToHex :: Transaction -> Aff String
txToHex =
  liftEffect
    <<< map (byteArrayToHex <<< Serialization.toBytes <<< asOneOf)
    <<< Serialization.convertTransaction

class Connection a where
  enable :: Aff a
  getWalletAddress :: a -> Aff (Maybe Address)
  getCollateral :: a -> Aff (Maybe TransactionUnspentOutput)
  signTx :: a -> Transaction -> Aff (Maybe Transaction)
  signTxBytes :: a -> ByteArray -> Aff (Maybe ByteArray)
  fromHexString :: (a -> Effect (Promise String)) -> a -> Aff (Maybe ByteArray)
  fromMaybeHexString
    :: (a -> Effect (Promise (Maybe String))) -> a -> Aff (Maybe ByteArray)

instance namiConnection :: Connection NamiConnection where
  enable = Promise.toAffE $ _enableNami

  getWalletAddress nami = fromHexString _getNamiAddress nami <#>
    (_ >>= addressFromBytes)

  getCollateral nami = fromMaybeHexString getNamiCollateral nami >>=
    case _ of
      Nothing -> pure Nothing
      Just bytes -> do
        liftEffect $
          Deserialization.UnspentOuput.convertUnspentOutput
            <$> fromBytesEffect bytes

  signTx :: NamiConnection -> Transaction -> Aff (Maybe Transaction)
  signTx nami tx = do
    txHex <- txToHex tx
    fromHexString (_signTxNami txHex) nami >>= case _ of
      Nothing -> pure Nothing
      Just bytes -> map (combineWitnessSet tx) <$> liftEffect
        ( Deserialization.WitnessSet.convertWitnessSet
            <$> fromBytesEffect bytes
        )
    where
    -- We have to combine the newly returned witness set with the existing one
    -- Otherwise, any datums, etc... won't be retained
    combineWitnessSet :: Transaction -> TransactionWitnessSet -> Transaction
    combineWitnessSet (Transaction tx'@{ witnessSet: oldWits }) newWits =
      Transaction $ tx' { witnessSet = oldWits <> newWits }

  signTxBytes :: NamiConnection -> ByteArray -> Aff (Maybe ByteArray)
  signTxBytes nami txBytes = do
    fromHexString (_signTxNami (byteArrayToHex txBytes)) nami >>= case _ of
      Nothing -> pure Nothing
      Just witBytes -> Just <$> liftEffect (_attachSignature txBytes witBytes)

  fromHexString act = map hexToByteArray <<< Promise.toAffE <<< act

  fromMaybeHexString act =
    map (flip bind hexToByteArray) <<< Promise.toAffE <<< act

instance geroConnection :: Connection GeroConnection where
  enable = Promise.toAffE $ _enableGero

  getWalletAddress gero = fromHexString _getGeroAddress gero <#>
    (_ >>= addressFromBytes)

  getCollateral gero = fromMaybeHexString getGeroCollateral gero >>=
    case _ of
      Nothing -> pure Nothing
      Just bytes -> do
        liftEffect $
          Deserialization.UnspentOuput.convertUnspentOutput
            <$> fromBytesEffect bytes

  signTx gero tx = do
    txHex <- txToHex tx
    fromHexString (_signTxGero txHex) gero >>= case _ of
      Nothing -> pure Nothing
      Just bytes -> map (combineWitnessSet tx) <$> liftEffect
        ( Deserialization.WitnessSet.convertWitnessSet
            <$> fromBytesEffect bytes
        )
    where
    -- We have to combine the newly returned witness set with the existing one
    -- Otherwise, any datums, etc... won't be retained
    combineWitnessSet :: Transaction -> TransactionWitnessSet -> Transaction
    combineWitnessSet (Transaction tx'@{ witnessSet: oldWits }) newWits =
      Transaction $ tx' { witnessSet = oldWits <> newWits }

  signTxBytes gero txBytes = do
    fromHexString (_signTxGero (byteArrayToHex txBytes)) gero >>=
      case _ of
        Nothing -> pure Nothing
        Just witBytes -> Just <$> liftEffect (_attachSignature txBytes witBytes)

  fromHexString act = map hexToByteArray <<< Promise.toAffE <<< act

  fromMaybeHexString act =
    map (flip bind hexToByteArray) <<< Promise.toAffE <<< act

-------------------------------------------------------------------------------
-- Nami FFI stuff
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

foreign import _attachSignature
  :: ByteArray -- CBOR bytes of tx
  -> ByteArray -- CBOR bytes of witness set
  -> Effect (ByteArray)

-------------------------------------------------------------------------------
-- Gero FFI stuff
-------------------------------------------------------------------------------
foreign import data GeroConnection :: Type

foreign import _enableGero :: Effect (Promise GeroConnection)

foreign import _getGeroAddress :: GeroConnection -> Effect (Promise String)

foreign import _getGeroCollateral
  :: MaybeFfiHelper -> GeroConnection -> Effect (Promise (Maybe String))

getGeroCollateral :: GeroConnection -> Effect (Promise (Maybe String))
getGeroCollateral = _getGeroCollateral maybeFfiHelper

foreign import _signTxGero
  :: String -- Hex-encoded cbor of tx
  -> GeroConnection
  -> Effect (Promise String)
