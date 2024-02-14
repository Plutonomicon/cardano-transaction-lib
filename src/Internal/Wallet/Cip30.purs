module Ctl.Internal.Wallet.Cip30
  ( Cip30Wallet
  , DataSignature
  , mkCip30WalletAff
  ) where

import Prelude

import Cardano.Wallet.Cip30 (Api)
import Cardano.Wallet.Cip30.TypeSafe (APIError)
import Cardano.Wallet.Cip30.TypeSafe as Cip30
import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, liftMaybe, throwError)
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
import Ctl.Internal.Helpers (liftM)
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
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import JS.BigInt (fromInt) as BigInt

type DataSignature =
  { key :: CborBytes
  , signature :: CborBytes
  }

-- Please update Cip30Mock when you add or remove methods here.
-- | A simplified internal view of CIP-30 API that wraps `Api` from
-- | `purescript-cip30`.
-- |
-- | - We hardcode the collateral amount to 5 ADA
-- | - We always request all UTxOs in `getUtxos`
-- | - We don't support querying of supported CIP-30 extensions
-- | - We don't support getting wallet icon, name and apiVersion
-- | - We don't support `isEnabled` call
-- |
-- | Use `purescript-cip30` for these: `connection` field contains the API handle.
type Cip30Wallet =
  { -- A reference to a connection with the wallet, i.e. the result of calling
    -- `window.cardano[walletName].enable()`,
    connection :: Api
  -- Returns the network id of the currently connected account. 0 is for any
  -- of the test networks, and 1 is mainnet.
  , getNetworkId :: Aff Int
  -- Returns a list of all UTXOs controlled by the wallet.
  , getUtxos :: Aff (Maybe (Array TransactionUnspentOutput))
  -- Get the collateral UTxO associated with the Nami wallet
  , getCollateral ::
      Aff (Maybe (Array TransactionUnspentOutput))
  -- Get combination of all available UTxOs
  , getBalance :: Aff Value
  -- Get the address associated with the wallet (Nami does not support
  -- multiple addresses)
  , getUsedAddresses :: Aff (Array Address)
  -- Sign a transaction with the given wallet
  -- Returns a list of unused addresses controlled by the wallet.
  , getUnusedAddresses :: Aff (Array Address)
  -- Returns an address owned by the wallet that should be used as a change
  -- address to return leftover assets during transaction creation back to
  -- the connected wallet.
  , getChangeAddress :: Aff Address
  -- Returns the reward addresses owned by the wallet. This can return multiple
  -- addresses e.g. CIP-0018
  , getRewardAddresses :: Aff (Array Address)
  , signTx :: Transaction -> Aff Transaction
  , signData :: Address -> RawBytes -> Aff DataSignature
  }

mkCip30WalletAff
  :: Api
  -- ^ A function to get wallet connection
  -> Aff Cip30Wallet
mkCip30WalletAff connection = do
  pure
    { connection
    , getNetworkId: Cip30.getNetworkId connection >>= handleApiError
    , getUtxos: getUtxos connection
    , getCollateral: getCollateral connection
    , getBalance: getBalance connection
    , getUsedAddresses: getUsedAddresses connection
    , getUnusedAddresses: getUnusedAddresses connection
    , getChangeAddress: getChangeAddress connection
    , getRewardAddresses: getRewardAddresses connection
    , signTx: signTx connection
    , signData: signData connection
    }

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

txToHex :: Transaction -> Effect String
txToHex =
  map (byteArrayToHex <<< unwrap <<< Serialization.toBytes)
    <<< Serialization.convertTransaction

handleApiError
  :: forall a. Variant (apiError :: APIError, success :: a) -> Aff a
handleApiError = match
  { success: pure :: a -> Aff a, apiError: show >>> throw >>> liftEffect }

getUnusedAddresses :: Api -> Aff (Array Address)
getUnusedAddresses conn = do
  Cip30.getUnusedAddresses conn >>= handleApiError >>=
    traverse
      ( liftM (error "CIP-30 getUnusedAddresses returned non-address") <<<
          hexStringToAddress
      )

getChangeAddress :: Api -> Aff Address
getChangeAddress conn = Cip30.getChangeAddress conn >>= handleApiError >>=
  liftM (error "CIP-30 getChangeAddress returned non-address") <<<
    hexStringToAddress

getRewardAddresses :: Api -> Aff (Array Address)
getRewardAddresses conn =
  Cip30.getRewardAddresses conn >>= handleApiError >>=
    traverse
      ( liftM (error "CIP-30 getRewardAddresses returned non-address") <<<
          hexStringToAddress
      )

getUsedAddresses :: Api -> Aff (Array Address)
getUsedAddresses conn = do
  result <- Cip30.getUsedAddresses conn Nothing
  result `flip match`
    { success: traverse
        ( liftM (error "CIP-30 getUsedAddresses returned non-address") <<<
            hexStringToAddress
        )
    , paginateError: show >>> throw >>> liftEffect
    , apiError: show >>> throw >>> liftEffect
    }

hexStringToAddress :: String -> Maybe Address
hexStringToAddress = fromBytes <<< rawBytesAsCborBytes <=< hexToRawBytes

defaultCollateralAmount :: Coin
defaultCollateralAmount = Coin $ BigInt.fromInt 5_000_000

-- | Get collateral using CIP-30 `getCollateral` method.
-- | Throws on `Promise` rejection by wallet, returns `Nothing` if no collateral
-- | is available.
getCollateral :: Api -> Aff (Maybe (Array TransactionUnspentOutput))
getCollateral conn = do
  mbUtxoStrs <- getCip30Collateral conn defaultCollateralAmount
  liftEffect $ for mbUtxoStrs \utxoStrs -> do
    for utxoStrs \utxoStr -> do
      liftM (error $ "CIP-30 getCollateral returned bad UTxO: " <> utxoStr) $
        Deserialization.UnspentOuput.convertUnspentOutput
          =<< fromBytes
          =<< hexToCborBytes utxoStr

getUtxos :: Api -> Aff (Maybe (Array TransactionUnspentOutput))
getUtxos conn = do
  result <- Cip30.getUtxos conn Nothing Nothing
  result `flip match`
    { success: \mbUtxoArray -> do
        liftEffect $ for mbUtxoArray $ \utxoArray -> for utxoArray \str -> do
          liftMaybe (error $ "CIP-30 getUtxos returned bad UTxO: " <> str) $
            hexToCborBytes str >>= fromBytes >>=
              Deserialization.UnspentOuput.convertUnspentOutput
    , paginateError: show >>> throw >>> liftEffect
    , apiError: show >>> throw >>> liftEffect
    }

signTx :: Api -> Transaction -> Aff Transaction
signTx conn tx = do
  txHex <- liftEffect $ txToHex tx
  result <- Cip30.signTx conn txHex true
  liftEffect $ result `flip match`
    { success:
        \hexString -> do
          bytes <- liftM (mkInvalidHexError hexString) $ hexToRawBytes
            hexString
          combineWitnessSet tx <$>
            ( Deserialization.WitnessSet.convertWitnessSet
                <$> fromBytesEffect (rawBytesAsCborBytes bytes)
            )
    , apiError: show >>> throw
    , txSignError: show >>> throw
    }
  where
  -- We have to combine the newly returned witness set with the existing one
  -- Otherwise, any datums, etc... won't be retained
  combineWitnessSet :: Transaction -> TransactionWitnessSet -> Transaction
  combineWitnessSet (Transaction tx'@{ witnessSet: oldWits }) newWits =
    Transaction $ tx' { witnessSet = oldWits <> newWits }

  mkInvalidHexError hexString = error $ "Unable to decode WitnessSet bytes: " <>
    hexString

-- | Supports : `BaseAddress`, `EnterpriseAddress`,
-- | `PointerAddress` and `RewardAddress`
signData :: Api -> Address -> RawBytes -> Aff DataSignature
signData conn address dat = do
  byteAddress <-
    liftMaybe
      (error "Can't convert Address to base, enterprise, pointer or reward")
      (fromBase <|> fromEnterprise <|> fromPointer <|> fromReward)
  result <- Cip30.signData conn (cborBytesToHex byteAddress)
    (rawBytesToHex dat)
  liftEffect $ result `flip match`
    { dataSignError: show >>> throw
    , apiError: show >>> throw
    , success: \signedData -> do
        key <- liftM byteError $ hexToCborBytes signedData.key
        signature <- liftM byteError $ hexToCborBytes signedData.signature
        pure { key: key, signature: signature }
    }
  where
  byteError = error "signData: hexToCborBytes failure"

  fromBase :: Maybe CborBytes
  fromBase = baseAddressBytes <$> baseAddressFromAddress address

  fromEnterprise :: Maybe CborBytes
  fromEnterprise = enterpriseAddressBytes <$>
    enterpriseAddressFromAddress address

  fromPointer :: Maybe CborBytes
  fromPointer = pointerAddressBytes <$> pointerAddressFromAddress address

  fromReward :: Maybe CborBytes
  fromReward = rewardAddressBytes <$> rewardAddressFromAddress address

getBalance :: Api -> Aff Value
getBalance conn = do
  Cip30.getBalance conn >>= handleApiError >>=
    liftM (error "CIP-30 getUsedAddresses returned non-address") <<<
      (hexToCborBytes >=> fromBytes >=> convertValue)

getCip30Collateral
  :: Api -> Coin -> Aff (Maybe (Array String))
getCip30Collateral conn requiredValue = do
  bigNumValue <- liftEffect $ maybe (throw convertError) pure
    $ BigNum.fromBigInt
    $ unwrap requiredValue
  let requiredValueStr = byteArrayToHex $ unwrap $ toBytes bigNumValue
  (Cip30.getCollateral conn requiredValueStr >>= handleApiError) `catchError`
    \err -> throwError $ error $
      "Failed to call `getCollateral`: " <> show err
  where
  convertError =
    "Unable to convert CIP-30 getCollateral required value: " <>
      show requiredValue
