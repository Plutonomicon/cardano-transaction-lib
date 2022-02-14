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
import Types.ByteArray (byteArrayToHex)
import Types.POSIXTimeRange
  ( Extended(NegInf, PosInf)
  , Interval(Interval)
  , LowerBound(LowerBound)
  , UpperBound(UpperBound)
  )
import Types.Transaction
  ( NetworkId(Testnet)
  , Transaction(Transaction)
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  , emptyTransactionWitnessSet
  )
import Types.UnbalancedTransaction (UnbalancedTx(UnbalancedTx))
import Types.Value as Value
import Undefined (undefined)
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
            -- FIXME
            , fee: Value.mkCoin 0
            , network_id: Just Testnet
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
        , witness_set: emptyTransactionWitnessSet
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

