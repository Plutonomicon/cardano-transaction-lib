module Ctl.Internal.CardanoCli
  ( CardanoNodeInstance
  , CardanoCliTxOutInfo
  , queryUtxosViaCardanoCli
  , cardanoCliTxOutInfoToUtxo
  ) where

import Contract.Prelude

import Cardano.Serialization.Lib as CSL
import Cardano.Types as Cardano.Types
import Cardano.Types.Address as Cardano.Types.Address
import Cardano.Types.AssetName as Cardano.Types.AssetName
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Value as Cardano.Types.Value
import Contract.Value as Contract.Value
import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Control.Parallel (parallel, sequential)
import Ctl.Internal.Spawn as Ctl.Internal.Spawn
import Ctl.Internal.Testnet.Utils (annotateError)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.ByteArray (ByteArray)
import Data.ByteArray as Data.ByteArray
import Data.List as List
import Data.String (Pattern(Pattern), split, trim) as String
import Data.String.CodeUnits (fromCharArray) as String
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Exception (error)
import Node.Buffer as Node.Buffer
import Node.ChildProcess as Node.ChildProcess
import Node.Encoding as Node.Encoding
import Node.Path as Node.Path
import Node.Stream.Aff as Node.Stream.Aff
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic

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
      , amount
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
  , amount :: Cardano.Types.Value -- Coin
  }

-- | A line of the CLI output of 'cardano-cli query utxo' containing utxo information
newtype CliUtxo = CliUtxo String

parseTxOutIgnoringDatums
  :: CliUtxo -> Either Parsing.ParseError CardanoCliTxOutInfo
parseTxOutIgnoringDatums (CliUtxo src) = Parsing.runParser src do
  Parsing.String.Basic.skipSpaces
  txHash <- parseTxHash
  Parsing.String.Basic.skipSpaces
  txOutId <- parseTxOutputIndex
  let
    txDatumBegin =
      Parsing.String.string "TxOutDatumNone"
        <|> Parsing.String.string "TxOutDatumInline"
        <|> Parsing.String.string "TxOutDatumHash"

  Parsing.String.Basic.skipSpaces
  rawValues <- Parsing.Combinators.manyTill
    (parseAsset <* Parsing.String.string " + ")
    txDatumBegin
  value <-
    noteParser "Can't sum up asset amounts"
      $ Contract.Value.sum
      $ Array.fromFoldable rawValues
  pure
    { input: { txHash, txOutId }
    , amount: value
    }

-- | Queries address funds via cardano-cli and returns the first UTxO data.
-- Note, that it assumes no output datum and script ref.
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
    let
      parsingError utxos (Parsing.ParseError msg pos) = mconcat
        [ "Cannot parse UTxOs: "
        , show utxos
        , " because of "
        , msg
        , " at "
        , show pos
        ]
    case List.fromFoldable stdout of
      List.Cons _ (List.Cons _ utxos) ->
        map Array.fromFoldable
          <<< liftEither
          <<< lmap (error <<< parsingError utxos)
          $ for utxos (parseTxOutIgnoringDatums <<< CliUtxo)
      _ -> throwError $ error $ "Output is too short " <> show stdout

execCardanoCli
  :: Array String
  -> Aff
       { stdout :: Array String
       , process :: Node.ChildProcess.ChildProcess
       }
execCardanoCli params = annotateError "execCardanoCli" do
  let cmd = "cardano-cli " <> intercalate " " params
  { channels, process } <- Ctl.Internal.Spawn.exec cmd
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

-- * Parsing

parseTxOutputIndex :: Parsing.Parser String UInt
parseTxOutputIndex = parseNatural \n ->
  note ("Can't parse Tx output index: " <> n)
    $ UInt.fromString n

parseAsset :: Parsing.Parser String Cardano.Types.Value
parseAsset = do
  amount <- parseAmount
  Parsing.String.Basic.skipSpaces
  parseLovelace amount <|> parseNativeAsset amount
  where
  parseAmount = parseNatural \n ->
    note ("Can't parse asset amount: " <> n)
      $ BigNum.fromString n
  parseLovelace amount = ado
    void $ Parsing.String.string "lovelace"
    in Cardano.Types.Value.coinToValue $ wrap amount
  parseNativeAsset amount = ado
    cs <- parseScriptHash
    void $ Parsing.String.char '.'
    tn <- parseAssetName
    in Contract.Value.singleton cs tn amount

parseTxHash :: Parsing.Parser String Cardano.Types.TransactionHash
parseTxHash = map wrap
  <<< noteParser "Cannot parse tx hash from byte array"
  <<< CSL.fromBytes
  =<< parseHex

parseScriptHash :: Parsing.Parser String Cardano.Types.ScriptHash
parseScriptHash = parseHex >>= \bytes ->
  map wrap
    $ noteParser ("Cannot parse script hash from byte array" <> show bytes)
    $ CSL.fromBytes bytes

parseAssetName :: Parsing.Parser String Cardano.Types.AssetName
parseAssetName =
  noteParser "Cannot create asset name from the parsed bytes"
    <<< Cardano.Types.AssetName.mkAssetName
    =<< parseHex

makeString :: forall f. Foldable f => f Char -> String
makeString = String.fromCharArray <<< Array.fromFoldable

parseNatural :: forall n. (String -> Either String n) -> Parsing.Parser String n
parseNatural toNumber = Parsing.liftEither
  <<< toNumber
  <<< makeString
  =<< Parsing.Combinators.many Parsing.String.Basic.digit

noteParser :: forall s a. String -> Maybe a -> Parsing.Parser s a
noteParser err = Parsing.liftEither <<< note err

parseHex :: Parsing.Parser String ByteArray
parseHex =
  noteParser "Cannot parse hex-encoded byte array"
    <<< Data.ByteArray.hexToByteArray
    <<< makeString
    =<< Parsing.Combinators.many Parsing.String.Basic.hexDigit
