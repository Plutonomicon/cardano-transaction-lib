module Plutip.Types
  ( ClusterStartupParameters
  , ErrorMessage
  , FilePath
  , InitialUTxO
  , InitialUTxODistribution
  , InitialUTxOWithStakeKey
  , PlutipConfig
  , PostgresConfig
  , ClusterStartupRequest(ClusterStartupRequest)
  , PrivateKeyResponse(PrivateKeyResponse)
  , ClusterStartupFailureReason
      ( ClusterIsRunningAlready
      , NegativeLovelaces
      , NodeConfigNotFound
      )
  , StartClusterResponse
      ( ClusterStartupFailure
      , ClusterStartupSuccess
      )
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse(StopClusterSuccess, StopClusterFailure)
  , UtxoAmount
  , class UtxoDistribution
  , decodeWallets
  , keyWallets
  , encodeDistribution
  , transferFundsFromEnterpriseToBase
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
  ( PaymentPubKeyHash
  , StakePubKeyHash
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , payPubKeyHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.Prelude (foldM, foldMap, null)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionOutput(TransactionOutput)
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
import Data.List (List, (:))
import Data.Log.Level (LogLevel)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Plutus.Types.Transaction (Utxo)
import QueryM.ServerConfig (ServerConfig)
import Serialization (privateKeyFromBytes)
import Serialization.Types (PrivateKey)
import Type.Prelude (Proxy(Proxy))
import Types.ByteArray (hexToByteArray)
import Types.RawBytes (RawBytes(RawBytes))
import Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey(PrivatePaymentKey)
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
  decodeWallets :: distr -> Array PrivateKeyResponse -> Maybe wallets
  keyWallets :: Proxy distr -> wallets -> Array KeyWallet

instance UtxoDistribution Unit Unit where
  encodeDistribution _ = []
  decodeWallets _ _ = pure unit
  keyWallets _ _ = []

instance UtxoDistribution InitialUTxO KeyWallet where
  encodeDistribution amounts = [ amounts ]
  decodeWallets _ [ PrivateKeyResponse key ] =
    Just $ privateKeysToKeyWallet (PrivatePaymentKey key) Nothing
  decodeWallets _ _ = Nothing
  keyWallets _ wallet = [ wallet ]

instance UtxoDistribution InitialUTxOWithStakeKey KeyWallet where
  encodeDistribution (InitialUTxOWithStakeKey _ amounts) = [ amounts ]
  decodeWallets (InitialUTxOWithStakeKey stake _) [ PrivateKeyResponse key ] =
    Just $ privateKeysToKeyWallet (PrivatePaymentKey key) (Just stake)
  decodeWallets _ _ = Nothing
  keyWallets _ wallet = [ wallet ]

instance
  ( UtxoDistribution headSpec headWallets
  , UtxoDistribution restSpec restWallets
  ) =>
  UtxoDistribution (headSpec /\ restSpec) (headWallets /\ restWallets) where
  encodeDistribution (distr /\ rest) =
    encodeDistribution distr <> encodeDistribution rest
  decodeWallets (distr /\ rest) =
    Array.uncons >=>
      \{ head, tail } -> do
        wallet <- decodeWallets distr [ head ]
        Tuple wallet <$> decodeWallets rest tail
  keyWallets _ (headWallets /\ restWallets) =
    (keyWallets (Proxy :: Proxy headSpec) headWallets)
      <> (keyWallets (Proxy :: Proxy restSpec) restWallets)

type WalletInfo =
  { utxos :: Utxo
  , payPkh :: PaymentPubKeyHash
  , stakePkh :: StakePubKeyHash
  , wallet :: KeyWallet
  }

-- | For each wallet which includes a stake key, transfer the value of
-- | the utxos at its enterprise address to its base address.
transferFundsFromEnterpriseToBase
  :: forall (r :: Row Type)
   . PrivatePaymentKey
  -> Array KeyWallet
  -> Contract r Unit
transferFundsFromEnterpriseToBase ourKey wallets = do
  -- Get all utxos and key hashes at all wallets containing a stake key
  walletsInfo <- foldM addStakeKeyWalletInfo mempty wallets
  unless (null walletsInfo) do
    let ourWallet = privateKeysToKeyWallet ourKey Nothing
    ourAddr <- liftedM "Could not get our address"
      $ withKeyWallet ourWallet getWalletAddress
    ourUtxos <- liftedM "Could not find our utxos" $ utxosAt ourAddr
    ourPkh <- withKeyWallet ourWallet $ liftedM "Could not get our payment pkh"
      ownPaymentPubKeyHash
    let
      lookups = Lookups.unspentOutputs (unwrap ourUtxos)
        <> foldMap (_.utxos >>> Lookups.unspentOutputs) walletsInfo

      constraints :: Constraints.TxConstraints Unit Unit
      constraints = Constraints.mustBeSignedBy ourPkh
        <> foldMap constraintsForWallet walletsInfo
    unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    signedTx <- liftedE $ withKeyWallet ourWallet $
      balanceAndSignTxE unbalancedTx
    signedTx' <- foldM
      ( \tx { wallet } -> liftedM "Could not sign" $ withKeyWallet wallet $
          signTransaction tx
      )
      (unwrap signedTx)
      walletsInfo
    txHash <- submit (wrap signedTx')
    awaitTxConfirmed txHash
  where
  constraintsForWallet :: WalletInfo -> Constraints.TxConstraints Unit Unit
  constraintsForWallet { utxos, payPkh, stakePkh } =
    -- It's necessary to include `mustBeSignedBy`, we get a
    -- `feeTooSmall` error otherwise
    Constraints.mustBeSignedBy payPkh <>
      foldMapWithIndex
        ( \input (TransactionOutput { amount }) ->
            Constraints.mustPayToPubKeyAddress payPkh stakePkh amount
              <> Constraints.mustSpendPubKeyOutput input
        )
        utxos

  addStakeKeyWalletInfo
    :: List WalletInfo
    -> KeyWallet
    -> Contract r (List WalletInfo)
  addStakeKeyWalletInfo walletsInfo wallet = withKeyWallet wallet $
    ownStakePubKeyHash >>= case _ of
      Nothing -> pure walletsInfo
      Just stakePkh -> do
        payPkh <- liftedM "Could not get payment pubkeyhash" $
          ownPaymentPubKeyHash
        networkId <- getNetworkId
        addr <- liftContractM "Could not get wallet address" $
          payPubKeyHashEnterpriseAddress networkId payPkh
        utxos' <- liftedM "Could not find utxos" $ utxosAt addr
        pure $ { utxos: unwrap utxos', payPkh, stakePkh, wallet } : walletsInfo

withStakeKey :: PrivateStakeKey -> InitialUTxO -> InitialUTxOWithStakeKey
withStakeKey = InitialUTxOWithStakeKey
