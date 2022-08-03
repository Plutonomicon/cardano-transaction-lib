module Plutip.Types
  ( ClusterStartupFailureReason(..)
  , ClusterStartupParameters
  , ClusterStartupRequest(..)
  , ErrorMessage
  , FilePath
  , InitialUTxO
  , InitialUTxODistribution
  , InitialUTxOWithStakeKey
  , PlutipConfig
  , PostgresConfig
  , PrivateKeyResponse(..)
  , StartClusterResponse(..)
  , StopClusterRequest(..)
  , StopClusterResponse(..)
  , UtxoAmount
  , class UtxoDistribution
  , decodeWallets
  , encodeDistribution
  , withStakeKey
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch, UnexpectedValue)
  , decodeAeson
  , encodeAeson'
  , toStringifiedNumbersJson
  , (.:)
  )
import Contract.Address
  ( getWalletAddress
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  )
import Contract.Monad (Contract, liftedE, liftedM, throwContractError)
import Contract.Prelude (isJust, mconcat)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionOutput(..)
  , awaitTxConfirmed
  , balanceAndSignTxE
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Wallet (withKeyWallet)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Either (Either(Left), note)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Effect.Class.Console (log)
import QueryM.ServerConfig (ServerConfig)
import Serialization (privateKeyFromBytes)
import Serialization.Types (PrivateKey)
import Types.ByteArray (hexToByteArray)
import Types.RawBytes (RawBytes(RawBytes))
import Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey(..)
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )

type PlutipConfig =
  { host :: String
  , port :: UInt
  , logLevel :: LogLevel
  -- Server configs are used to deploy the corresponding services:
  , ogmiosConfig :: ServerConfig
  , ogmiosDatumCacheConfig :: ServerConfig
  , ctlServerConfig :: ServerConfig
  -- Should be synchronized with `defaultConfig.postgres` in `flake.nix`
  , postgresConfig :: PostgresConfig
  }

type PostgresConfig =
  { host :: String
  , port :: UInt
  , user :: String
  , password :: String
  , dbname :: String
  }

type FilePath = String

type ErrorMessage = String

-- | UTxO amount in Lovelaces
type UtxoAmount = BigInt

type InitialUTxO = Array UtxoAmount

data InitialUTxOWithStakeKey =
  InitialUTxOWithStakeKey PrivateStakeKey InitialUTxO

type InitialUTxODistribution = Array InitialUTxO

newtype ClusterStartupRequest = ClusterStartupRequest
  { keysToGenerate :: InitialUTxODistribution }

derive newtype instance EncodeAeson ClusterStartupRequest

newtype PrivateKeyResponse = PrivateKeyResponse PrivateKey

derive instance Newtype PrivateKeyResponse _
derive instance Generic PrivateKeyResponse _

instance Show PrivateKeyResponse where
  show _ = "(PrivateKeyResponse \"<private key>\")"

instance DecodeAeson PrivateKeyResponse where
  decodeAeson json = do
    cborStr <- decodeAeson json
    let splitted = String.splitAt 4 cborStr
    -- 5820 prefix comes from Cbor
    if splitted.before == "5820" then do
      cborBytes <- note err $ hexToByteArray splitted.after
      PrivateKeyResponse <$> note err (privateKeyFromBytes (RawBytes cborBytes))
    else Left err
    where
    err :: JsonDecodeError
    err = TypeMismatch "PrivateKey"

type ClusterStartupParameters =
  { privateKeys :: Array PrivateKeyResponse
  , nodeSocketPath :: FilePath
  , nodeConfigPath :: FilePath
  , keysDirectory :: FilePath
  }

data ClusterStartupFailureReason
  = ClusterIsRunningAlready
  | NegativeLovelaces
  | NodeConfigNotFound

derive instance Generic ClusterStartupFailureReason _

instance Show ClusterStartupFailureReason where
  show = genericShow

instance DecodeAeson ClusterStartupFailureReason where
  decodeAeson aeson = do
    decodeAeson aeson >>= case _ of
      "ClusterIsRunningAlready" -> do
        pure ClusterIsRunningAlready
      "NegativeLovelaces" -> pure NegativeLovelaces
      "NodeConfigNotFound" -> pure NodeConfigNotFound
      _ -> do
        Left (UnexpectedValue (toStringifiedNumbersJson aeson))

data StartClusterResponse
  = ClusterStartupFailure ClusterStartupFailureReason
  | ClusterStartupSuccess ClusterStartupParameters

derive instance Generic StartClusterResponse _

instance Show StartClusterResponse where
  show = genericShow

instance DecodeAeson StartClusterResponse where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    obj .: "tag" >>= case _ of
      "ClusterStartupSuccess" -> do
        contents <- obj .: "contents"
        ClusterStartupSuccess <$> decodeAeson contents
      "ClusterStartupFailure" -> do
        failure <- obj .: "contents"
        ClusterStartupFailure <$> decodeAeson failure
      _ -> do
        Left (UnexpectedValue (toStringifiedNumbersJson aeson))

data StopClusterRequest = StopClusterRequest

derive instance Generic StopClusterRequest _

instance Show StopClusterRequest where
  show = genericShow

instance EncodeAeson StopClusterRequest where
  encodeAeson' _ = encodeAeson' ([] :: Array Int)

data StopClusterResponse = StopClusterSuccess | StopClusterFailure ErrorMessage

derive instance Generic StopClusterResponse _

instance Show StopClusterResponse where
  show = genericShow

instance DecodeAeson StopClusterResponse where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    obj .: "tag" >>= case _ of
      "StopClusterSuccess" -> pure StopClusterSuccess
      "StopClusterFailure" -> do
        failure <- obj .: "contents"
        StopClusterFailure <$> decodeAeson failure
      _ -> do
        Left (UnexpectedValue (toStringifiedNumbersJson aeson))

-- | A type class that implements a type-safe interface for specifying UTXO
-- | distribution for wallets.
-- | Number of wallets in distribution specification matches the number of
-- | wallets provided to the user.
class UtxoDistribution distr wallets | distr -> wallets where
  encodeDistribution :: distr -> Array (Array UtxoAmount)
  decodeWallets
    :: PrivatePaymentKey
    -> distr
    -> Array PrivateKeyResponse
    -> Contract () wallets

instance UtxoDistribution Unit Unit where
  encodeDistribution _ = []
  decodeWallets _ _ _ = pure unit

instance UtxoDistribution InitialUTxO KeyWallet where
  encodeDistribution amounts = [ amounts ]
  decodeWallets ourKey _ [ key ] = decodeWallet ourKey key Nothing
  decodeWallets _ _ _ = wrongNumberPrivateKeysError

instance UtxoDistribution InitialUTxOWithStakeKey KeyWallet where
  encodeDistribution (InitialUTxOWithStakeKey _ amounts) = [ amounts ]
  decodeWallets ourKey (InitialUTxOWithStakeKey stake _) [ key ] =
    decodeWallet ourKey key (Just stake)
  decodeWallets _ _ _ = wrongNumberPrivateKeysError

instance
  ( UtxoDistribution headSpec headWallets
  , UtxoDistribution restSpec restWallets
  ) =>
  UtxoDistribution (headSpec /\ restSpec) (headWallets /\ restWallets) where
  encodeDistribution (distr /\ rest) =
    encodeDistribution distr <> encodeDistribution rest
  decodeWallets ourKey (distr /\ rest) =
    Array.uncons >>> case _ of
      Nothing -> wrongNumberPrivateKeysError
      Just { head, tail } -> do
        wallet <- decodeWallets ourKey distr [ head ]
        Tuple wallet <$> decodeWallets ourKey rest tail

-- | Get a wallet for a single private key response, and if a stake
-- | key is provided for it, transfer the funds allocated by plutip to
-- | the address combining the payment key and the stake key
decodeWallet
  :: PrivatePaymentKey
  -> PrivateKeyResponse
  -> Maybe PrivateStakeKey
  -> Contract () KeyWallet
decodeWallet ourKey (PrivateKeyResponse key) stake = do
  let
    paymentKey = PrivatePaymentKey key
    wallet = privateKeysToKeyWallet paymentKey stake
  when (isJust stake) do
    let
      ourWallet = privateKeysToKeyWallet ourKey Nothing
      payKeyOnlyWallet = privateKeysToKeyWallet paymentKey Nothing
    payKeyOnlyAddr <- liftedM "Could not get address"
      $ withKeyWallet payKeyOnlyWallet getWalletAddress
    ourAddr <- liftedM "Could not get our address"
      $ withKeyWallet ourWallet getWalletAddress
    payKeyOnlyUtxos <- liftedM "Could not find utxos" $ utxosAt payKeyOnlyAddr
    ourUtxos <- liftedM "Could not find utxos" $ utxosAt ourAddr

    log $ show payKeyOnlyUtxos
    log $ show ourUtxos

    payPkh <- withKeyWallet wallet $ liftedM
      "Could not get payment pkh"
      ownPaymentPubKeyHash
    stakePkh <- withKeyWallet wallet $ liftedM
      "Could not get stake pkh"
      ownStakePubKeyHash
    ourPkh <- withKeyWallet ourWallet $ liftedM
      "Could not get our payment pkh"
      ownPaymentPubKeyHash

    let
      lookups = mconcat
        [ Lookups.unspentOutputs $ unwrap payKeyOnlyUtxos
        , Lookups.unspentOutputs $ unwrap ourUtxos
        ]

      constraints :: Constraints.TxConstraints Unit Unit
      constraints =
        -- Both `mustBeSignedBy`s need to be included, otherwise
        -- there's a `feeTooSmall` error
        Constraints.mustBeSignedBy payPkh
          <> Constraints.mustBeSignedBy ourPkh
          <>
            foldMapWithIndex
              ( \input (TransactionOutput output) ->
                  Constraints.mustPayToPubKeyAddress payPkh stakePkh
                    output.amount
                    <> Constraints.mustSpendPubKeyOutput input
              )
              (unwrap payKeyOnlyUtxos)
    unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    log $ show unbalancedTx
    signedTx <- liftedE $ withKeyWallet ourWallet $
      balanceAndSignTxE unbalancedTx
    signedTx' <- liftedM "Could not sign" $ withKeyWallet payKeyOnlyWallet $
      signTransaction (unwrap signedTx)
    txHash <- submit (wrap signedTx')
    awaitTxConfirmed txHash

  pure wallet

withStakeKey :: PrivateStakeKey -> InitialUTxO -> InitialUTxOWithStakeKey
withStakeKey = InitialUTxOWithStakeKey

wrongNumberPrivateKeysError :: forall a. Contract () a
wrongNumberPrivateKeysError = throwContractError
  "Wrong number of private keys provided while decoding wallets. Please report as a bug."
