module Examples.Nami.Pkh2Pkh (main) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (runReaderT)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.Class (liftAff)
import QueryM (QueryM, defaultServerConfig, getWalletAddress, utxosAt)
import Types.POSIXTimeRange
  ( Extended(..)
  , Interval(..)
  , LowerBound(..)
  , UpperBound(..)
  )
import Types.Transaction
  ( NetworkId(..)
  , Transaction(..)
  , TransactionOutput(..)
  , TxBody(..)
  , emptyTransactionWitnessSet
  )
import Types.UnbalancedTransaction (UnbalancedTx(..))
import Types.Value as Value
import Undefined (undefined)
import Wallet (mkNamiWalletAff)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  tx <- runReaderT
    buildTransaction
    { ws: {-TODO-}  undefined
    , wallet
    , serverConfig: defaultServerConfig
    }
  undefined

buildTransaction :: QueryM Transaction
buildTransaction = undefined

buildUnbalancedTransaction :: QueryM UnbalancedTx
buildUnbalancedTransaction = do
  ownAddress <- mthrow "Failed to get wallet address" getWalletAddress
  inputs <-
    map fst
      <<< Map.toUnfoldable
      <<< unwrap
      <$> mthrow "Failed to get own utxos" (utxosAt ownAddress)
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
            , fee: undefined
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

