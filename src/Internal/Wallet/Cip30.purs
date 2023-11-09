module Ctl.Internal.Wallet.Cip30
  ( Cip30Connection
  , Cip30Wallet
  , DataSignature
  , mkCip30WalletAff
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, liftMaybe, throwError)
import Control.Promise (Promise, toAffE)
import Control.Promise as Promise
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction(Transaction)
  , TransactionWitnessSet
  )
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import Ctl.Internal.Cardano.Types.Value (Coin(Coin), Value)
import Ctl.Internal.Deserialization.FromBytes (fromBytes, fromBytesEffect)
import Ctl.Internal.Deserialization.UnspentOutput (convertValue)
import Ctl.Internal.Deserialization.UnspentOutput as Deserialization.UnspentOuput
import Ctl.Internal.Deserialization.WitnessSet as Deserialization.WitnessSet
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Serialization (convertTransaction, toBytes) as Serialization
import Ctl.Internal.Serialization.Address
  ( Address
  , baseAddressBytes
  , baseAddressFromAddress
  , enterpriseAddressBytes
  , enterpriseAddressFromAddress
  , pointerAddressBytes
  , pointerAddressFromAddress
  , rewardAddressBytes
  , rewardAddressFromAddress
  )
import Ctl.Internal.Serialization.ToBytes (toBytes)
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.CborBytes
  ( CborBytes
  , cborBytesToHex
  , hexToCborBytes
  , rawBytesAsCborBytes
  )
import Ctl.Internal.Types.RawBytes (RawBytes, hexToRawBytes, rawBytesToHex)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import JS.BigInt (fromInt) as BigInt

type DataSignature =
  { key :: CborBytes
  , signature :: CborBytes
  }

-- Please update Cip30Mock when you add or remove methods in this handle.
type Cip30Wallet =
  { -- A reference to a connection with the wallet, i.e. `window.cardano.nami`
    connection :: Cip30Connection
  -- Returns the network id of the currently connected account. 0 is for any
  -- of the test networks, and 1 is mainnet.
  , getNetworkId :: Cip30Connection -> Aff Int
  -- Returns a list of all UTXOs controlled by the wallet.
  , getUtxos :: Cip30Connection -> Aff (Maybe (Array TransactionUnspentOutput))
  -- Get the collateral UTxO associated with the Nami wallet
  , getCollateral ::
      Cip30Connection -> Aff (Maybe (Array TransactionUnspentOutput))
  -- Get combination of all available UTxOs
  , getBalance :: Cip30Connection -> Aff (Maybe Value)
  -- Get the address associated with the wallet (Nami does not support
  -- multiple addresses)
  , getWalletAddresses :: Cip30Connection -> Aff (Maybe (Array Address))
  -- Sign a transaction with the given wallet
  -- Returns a list of unused addresses controlled by the wallet.
  , getUnusedAddresses :: Cip30Connection -> Aff (Maybe (Array Address))
  -- Returns an address owned by the wallet that should be used as a change
  -- address to return leftover assets during transaction creation back to
  -- the connected wallet.
  , getChangeAddress :: Cip30Connection -> Aff (Maybe Address)
  -- Returns the reward addresses owned by the wallet. This can return multiple
  -- addresses e.g. CIP-0018
  , getRewardAddresses :: Cip30Connection -> Aff (Maybe (Array Address))
  , signTx :: Cip30Connection -> Transaction -> Aff (Maybe Transaction)
  , signData ::
      Cip30Connection -> Address -> RawBytes -> Aff (Maybe DataSignature)
  }

mkCip30WalletAff
  :: Effect (Promise Cip30Connection)
  -- ^ A function to get wallet connection
  -> Aff Cip30Wallet
mkCip30WalletAff enableWallet = do
  wallet <- toAffE enableWallet
  pure
    { connection: wallet
    , getNetworkId
    , getUtxos
    , getCollateral
    , getBalance
    , getWalletAddresses
    , getUnusedAddresses
    , getChangeAddress
    , getRewardAddresses
    , signTx
    , signData
    }

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

txToHex :: Transaction -> Aff String
txToHex =
  liftEffect
    <<< map (byteArrayToHex <<< unwrap <<< Serialization.toBytes)
    <<< Serialization.convertTransaction

getNetworkId :: Cip30Connection -> Aff Int
getNetworkId = toAffE <<< _getNetworkId

getUnusedAddresses :: Cip30Connection -> Aff (Maybe (Array Address))
getUnusedAddresses conn = toAffE (_getUnusedAddresses conn) <#>
  traverse hexStringToAddress

getChangeAddress :: Cip30Connection -> Aff (Maybe Address)
getChangeAddress conn = (toAffE $ _getChangeAddress conn) <#> hexStringToAddress

getRewardAddresses :: Cip30Connection -> Aff (Maybe (Array Address))
getRewardAddresses conn = toAffE (_getRewardAddresses conn) <#>
  traverse hexStringToAddress

getWalletAddresses :: Cip30Connection -> Aff (Maybe (Array Address))
getWalletAddresses conn = Promise.toAffE (_getAddresses conn) <#>
  traverse hexStringToAddress

hexStringToAddress :: String -> Maybe Address
hexStringToAddress = fromBytes <<< rawBytesAsCborBytes <=< hexToRawBytes

defaultCollateralAmount :: Coin
defaultCollateralAmount = Coin $ BigInt.fromInt 5_000_000

-- | Get collateral using CIP-30 `getCollateral` method.
-- | Throws on `Promise` rejection by wallet, returns `Nothing` if no collateral
-- | is available.
getCollateral :: Cip30Connection -> Aff (Maybe (Array TransactionUnspentOutput))
getCollateral conn = do
  mbUtxoStrs <- toAffE $ getCip30Collateral conn defaultCollateralAmount
  let
    (mbUtxoBytes :: Maybe (Array RawBytes)) =
      join $ map (traverse hexToRawBytes) mbUtxoStrs
  -- de-serialize UTxOs
  liftEffect $ for mbUtxoBytes \collateralUtxos -> do
    for collateralUtxos \bytes -> do
      maybe (throw "Unable to convert UTxO") pure =<<
        Deserialization.UnspentOuput.convertUnspentOutput
          <$> fromBytesEffect (rawBytesAsCborBytes bytes)

getUtxos :: Cip30Connection -> Aff (Maybe (Array TransactionUnspentOutput))
getUtxos conn = do
  mArrayStr <- toAffE $ _getUtxos maybeFfiHelper conn
  liftEffect $ for mArrayStr $ traverse \str -> do
    liftMaybe (error "Unable to convert UTxO") $
      hexToCborBytes str >>= fromBytes >>=
        Deserialization.UnspentOuput.convertUnspentOutput

signTx :: Cip30Connection -> Transaction -> Aff (Maybe Transaction)
signTx conn tx = do
  txHex <- txToHex tx
  fromHexString (_signTx txHex) conn >>= case _ of
    Nothing -> pure Nothing
    Just bytes -> map (combineWitnessSet tx) <$> liftEffect
      ( Deserialization.WitnessSet.convertWitnessSet
          <$> fromBytesEffect (rawBytesAsCborBytes bytes)
      )
  where
  -- We have to combine the newly returned witness set with the existing one
  -- Otherwise, any datums, etc... won't be retained
  combineWitnessSet :: Transaction -> TransactionWitnessSet -> Transaction
  combineWitnessSet (Transaction tx'@{ witnessSet: oldWits }) newWits =
    Transaction $ tx' { witnessSet = oldWits <> newWits }

-- | Supports : `BaseAddress`, `EnterpriseAddress`,
-- | `PointerAddress` and `RewardAddress`
signData :: Cip30Connection -> Address -> RawBytes -> Aff (Maybe DataSignature)
signData conn address dat = do
  byteAddress <-
    liftMaybe
      (error "Can't convert Address to base, enterprise, pointer or reward")
      (fromBase <|> fromEnterprise <|> fromPointer <|> fromReward)
  signedData <- toAffE $ _signData (cborBytesToHex byteAddress)
    (rawBytesToHex dat)
    conn
  pure $ do
    key <- hexToCborBytes signedData.key
    signature <- hexToCborBytes signedData.signature
    pure { key: key, signature: signature }
  where
  fromBase :: Maybe CborBytes
  fromBase = baseAddressBytes <$> baseAddressFromAddress address

  fromEnterprise :: Maybe CborBytes
  fromEnterprise = enterpriseAddressBytes <$>
    enterpriseAddressFromAddress address

  fromPointer :: Maybe CborBytes
  fromPointer = pointerAddressBytes <$> pointerAddressFromAddress address

  fromReward :: Maybe CborBytes
  fromReward = rewardAddressBytes <$> rewardAddressFromAddress address

getBalance :: Cip30Connection -> Aff (Maybe Value)
getBalance wallet = do
  fromHexString _getBalance wallet <#> \mbBytes -> do
    bytes <- mbBytes
    fromBytes (rawBytesAsCborBytes bytes) >>= convertValue

fromHexString
  :: (Cip30Connection -> Effect (Promise String))
  -> Cip30Connection
  -> Aff (Maybe RawBytes)
fromHexString act = map hexToRawBytes <<< Promise.toAffE <<< act

-------------------------------------------------------------------------------
-- FFI stuff
-------------------------------------------------------------------------------
foreign import data Cip30Connection :: Type

foreign import _getNetworkId
  :: Cip30Connection
  -> Effect (Promise Int)

foreign import _getUtxos
  :: MaybeFfiHelper
  -> Cip30Connection
  -> Effect (Promise (Maybe (Array String)))

foreign import _getCollateral
  :: MaybeFfiHelper
  -> Cip30Connection
  -> String
  -> Effect (Promise (Maybe (Array String)))

getCip30Collateral
  :: Cip30Connection -> Coin -> Effect (Promise (Maybe (Array String)))
getCip30Collateral conn requiredValue = do
  bigNumValue <- maybe (throw convertError) pure
    $ BigNum.fromBigInt
    $ unwrap requiredValue
  let requiredValueStr = byteArrayToHex $ unwrap $ toBytes bigNumValue
  _getCollateral maybeFfiHelper conn requiredValueStr `catchError`
    \err -> throwError $ error $
      "Failed to call `getCollateral`: " <> show err
  where
  convertError =
    "Unable to convert CIP-30 getCollateral required value: " <>
      show requiredValue

foreign import _getBalance :: Cip30Connection -> Effect (Promise String)

foreign import _getAddresses
  :: Cip30Connection
  -> Effect
       ( Promise
           ( Array
               String
           )
       )

foreign import _getUnusedAddresses
  :: Cip30Connection
  -> Effect (Promise (Array String))

foreign import _getChangeAddress
  :: Cip30Connection
  -> Effect (Promise String)

foreign import _getRewardAddresses
  :: Cip30Connection
  -> Effect (Promise (Array String))

foreign import _signTx
  :: String -- Hex-encoded cbor of tx
  -> Cip30Connection
  -> Effect (Promise String)

foreign import _signData
  :: String -- Address
  -> String -- Hex-encoded data
  -> Cip30Connection
  -> Effect (Promise { key :: String, signature :: String })
