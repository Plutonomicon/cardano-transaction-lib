--
-- This module demonstrates how the `QueryM` interface can be used to build a
-- transaction from scratch. It creates and balances an example transaction that
-- gets UTxOs from the user's wallet and sends two Ada back to the same wallet
-- address
--
-- * Prerequisites
--   - A Chromium-based browser
--
--   - A Nami wallet funded with test Ada ("tAda") and collateral set, If you need
--     tAda, visit https://testnets.cardano.org/en/testnets/cardano/tools/faucet/
--
-- * How to run
--
--   The `QueryM` interface requires several external services to be running. From
--   the repository root, run the following commands:
--
--   - `make run-testnet-node`
--      Starts a testnet Cardano node. May take some time to sync fully
--
--   - `make run-testnet-ogmios`
--      Starts the Ogmios service. Also needs to sync with the running node
--
--   - `make run-haskell-server`
--      Starts the external Haskell server that will perform the transaction
--      fee calculations (no sync required)
--
--   Once these services are *fully synced*, run:
--
--   - `npm run dev` and visit `localhost:4008`. You may be prompted to enable
--     access to your wallet if you have not run this example before. You will
--     also be prompted to sign the transaction using your Nami password

module Examples.Nami.Pkh2Pkh (main) where

import Prelude

import BalanceTx (balanceTxM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (runReaderT)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Either (either)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Console as Console
import QueryM
  ( QueryM
  , defaultServerConfig
  , getWalletAddress
  , mkOgmiosWebSocketAff
  , utxosAt
  )
import Serialization as Serialization
import Serialization.Address (testnetId)
import Types.ByteArray (byteArrayToHex)
import Types.POSIXTimeRange
  ( Extended(NegInf, PosInf)
  , Interval(Interval)
  , LowerBound(LowerBound)
  , UpperBound(UpperBound)
  )
import Types.Transaction
  ( Transaction(Transaction)
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  )
import Types.UnbalancedTransaction (UnbalancedTx(UnbalancedTx))
import Types.Value as Value
import Untagged.Union (asOneOf)
import Wallet (mkNamiWalletAff)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  ws <- mkOgmiosWebSocketAff "ws:127.0.0.1:1337"
  tx <- runReaderT
    buildTransaction
    { ws
    , wallet
    , serverConfig: defaultServerConfig
    }
  liftEffect $
    Console.log
      <<< byteArrayToHex
      <<< Serialization.toBytes
      <<< asOneOf
      =<< Serialization.convertTransaction tx

buildTransaction :: QueryM Transaction
buildTransaction = either (throw <<< show) pure
  =<< balanceTxM
  =<< buildUnbalancedTransaction

buildUnbalancedTransaction :: QueryM UnbalancedTx
buildUnbalancedTransaction = do
  ownAddress <- mthrow "Failed to get wallet address" getWalletAddress
  inputs <-
    map fst
      <<< Map.toUnfoldable
      <<< unwrap
      <$> mthrow "Failed to get utxos" (utxosAt ownAddress)
  pure $ UnbalancedTx
    { transaction: Transaction
        { body: TxBody
            { inputs
            , outputs: Array.singleton $ TransactionOutput
                { address: ownAddress
                , amount: Value.lovelaceValueOf $ BigInt.fromInt 2000000
                , data_hash: Nothing
                }
            -- ??
            , fee: Value.mkCoin 0
            , network_id: Just testnetId
            , certs: Nothing
            , collateral: Nothing
            , auxiliary_data_hash: Nothing
            , mint: Nothing
            , required_signers: Nothing
            , script_data_hash: Nothing
            , ttl: Nothing
            , update: Nothing
            , validity_start_interval: Nothing
            , withdrawals: Nothing
            }
        , is_valid: true
        , witness_set: mempty
        , auxiliary_data: Nothing
        }
    , requiredSignatories: Map.empty
    , utxoIndex: Map.empty
    , validityTimeRange: Interval
        { from: LowerBound NegInf true
        , to: UpperBound PosInf true
        }
    }

throw :: forall (a :: Type). String -> QueryM a
throw = liftAff <<< throwError <<< error

mthrow :: forall (a :: Type). String -> QueryM (Maybe a) -> QueryM a
mthrow msg act = maybe (throw msg) pure =<< act
