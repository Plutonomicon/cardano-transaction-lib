module Ctl.Internal.CardanoCli where

import Contract.Prelude

import Cardano.Serialization.Lib as CSL
import Cardano.Types as Cardano.Types
import Cardano.Types.Address as Cardano.Types.Address
import Cardano.Types.BigNum as BigNum
import Contract.Value as Contract.Value
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except (throwError)
import Control.Parallel (parallel, sequential)
import Ctl.Internal.Plutip.Spawn as Ctl.Internal.Plutip.Spawn
import Ctl.Internal.Plutip.Utils (annotateError)
import Data.Array as Array
import Data.ByteArray as Data.ByteArray
import Data.List as List
import Data.String as String
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Exception (error)
import Node.Buffer as Node.Buffer
import Node.ChildProcess as Node.ChildProcess
import Node.Encoding as Node.Encoding
import Node.Path as Node.Path
import Node.Stream.Aff as Node.Stream.Aff

type CardanoNodeInstance =
  { socketPath :: Node.Path.FilePath
  , testnetMagic :: Int
  }

cardanoCliTxOutInfoToUtxo
  :: Cardano.Types.Address
  -> CardanoCliTxOutInfo
  -> Cardano.Types.TransactionInput /\ Cardano.Types.TransactionOutput
cardanoCliTxOutInfoToUtxo address { input: { txHash, txOutId }, amount } =
  let
    txIn = Cardano.Types.TransactionInput
      { index: txOutId, transactionId: txHash }
    txOut = Cardano.Types.TransactionOutput
      { address
      , amount: Contract.Value.coinToValue amount
      , datum: Nothing
      , scriptRef: Nothing
      }
  in
    txIn /\ txOut

type CardanoCliTxOutInfo =
  { input ::
      { txHash :: Cardano.Types.TransactionHash
      , txOutId :: UInt
      }
  , amount :: Cardano.Types.Coin
  }

-- | A line of the CLI output of 'cardano-cli query utxo' containing utxo information
newtype CliUtxo = CliUtxo String

parseTxOut :: CliUtxo -> Maybe CardanoCliTxOutInfo
parseTxOut (CliUtxo src) = do
  let
    words = String.split (String.Pattern " ") src
    nonEmptyWords = Array.take 3 $ Array.filter (not <<< String.null) words
  { hash, id, amt } <- case nonEmptyWords of
    [ hash, id, amt ] -> Just { hash, id, amt }
    _ -> Nothing
  txHash <- map wrap <<< CSL.fromBytes =<< Data.ByteArray.hexToByteArray hash
  txOutId <- UInt.fromString id
  amount <- BigNum.fromString amt
  pure { input: { txHash, txOutId }, amount: wrap amount }

-- | Queries address funds via cardano-cli and returns the first UTxO data.
queryUtxosViaCardanoCli
  :: CardanoNodeInstance
  -> Cardano.Types.Address
  -> Aff (Array CardanoCliTxOutInfo)
queryUtxosViaCardanoCli { socketPath, testnetMagic } address =
  annotateError "queryUtxosViaCardanoCli" do
    { stdout } <- execCardanoCli
      [ "query"
      , "utxo"
      , "--socket-path"
      , socketPath
      , "--testnet-magic"
      , show testnetMagic
      , "--address"
      , Cardano.Types.Address.toBech32 address
      ]
    case List.fromFoldable stdout of
      List.Cons _ (List.Cons _ utxos) ->
        map Array.fromFoldable
          <<< liftMaybe (error $ "Cannot parse TxOuts" <> show utxos)
          $ for utxos (parseTxOut <<< CliUtxo)
      _ -> throwError $ error $ "Output is too short " <> show stdout

execCardanoCli
  :: Array String
  -> Aff
       { stdout :: Array String
       , process :: Node.ChildProcess.ChildProcess
       }
execCardanoCli params = annotateError "execCardanoCli" do
  let cmd = "cardano-cli " <> intercalate " " params
  log $ show { execCardanoCli: cmd }
  { channels, process } <- Ctl.Internal.Plutip.Spawn.exec cmd
  let
    bufferToLines =
      map (String.split (String.Pattern "\n") <<< String.trim)
        <<< liftEffect
        <<< Node.Buffer.toString Node.Encoding.UTF8
  output <- sequential ado
    stderr <- parallel
      $ map join
      <<< traverse bufferToLines
      <<< fst
      =<< Node.Stream.Aff.readAll channels.stderr
    stdout <- parallel
      $ map join
      $ traverse bufferToLines
      <<< fst
      =<< Node.Stream.Aff.readAll channels.stdout
    in { stderr, stdout }
  when (not $ Array.null output.stderr) do
    throwError
      $ error
      $ intercalate "\n" output.stderr
  pure { stdout: output.stdout, process }