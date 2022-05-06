module Deserialization.UnspentOutput
  ( convertUnspentOutput
  , mkTransactionUnspentOutput
  , newTransactionUnspentOutputFromBytes
  , convertInput
  , convertOutput
  ) where

import Prelude

import Data.Bitraversable (bitraverse, ltraverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Data.UInt as UInt
import Deserialization.BigNum (bigNumToBigInt)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization (toBytes)
import Serialization.Address (Address)
import Serialization.Hash (ScriptHash, scriptHashToBytes)
import Serialization.Types
  ( AssetName
  , Assets
  , BigNum
  , DataHash
  , MultiAsset
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  , TransactionUnspentOutput
  , Value
  )
import Types.ByteArray (ByteArray)
import Types.Transaction
  ( DataHash(DataHash)
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  ) as T
import Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) as T
import Cardano.Types.Value
  ( Coin(Coin)
  , CurrencySymbol
  , Value
  , mkCurrencySymbol
  , mkNonAdaAsset
  , mkValue
  ) as T
import Types.TokenName (TokenName, assetNameName, mkTokenName) as T
import Untagged.Union (asOneOf)

convertUnspentOutput
  :: TransactionUnspentOutput -> Maybe T.TransactionUnspentOutput
convertUnspentOutput tuo = do
  input <- convertInput $ getInput tuo
  output <- convertOutput $ getOutput tuo
  pure $ T.TransactionUnspentOutput { input, output }

convertInput :: TransactionInput -> Maybe T.TransactionInput
convertInput input = do
  index <- UInt.fromInt' $ getTransactionIndex input
  pure $ T.TransactionInput
    { transactionId: T.TransactionHash $ toBytes
        (asOneOf $ getTransactionHash input)
    , index
    }

convertOutput :: TransactionOutput -> Maybe T.TransactionOutput
convertOutput output = do
  amount <- convertValue $ getAmount output
  let
    address = getAddress output
    dataHash =
      getDataHash maybeFfiHelper output <#>
        asOneOf >>> toBytes >>> T.DataHash
  pure $ T.TransactionOutput { address, amount, dataHash }

convertValue :: Value -> Maybe T.Value
convertValue value = do
  coin <- bigNumToBigInt $ getCoin value
  -- multiasset is optional
  multiasset <- for (getMultiAsset maybeFfiHelper value) \multiasset -> do
    let
      -- get multiasset stored in `serialization-lib` types
      multiasset' =
        multiasset
          # extractMultiAsset Tuple >>> map (map (extractAssets Tuple))
          :: Array (ScriptHash /\ Array (AssetName /\ BigNum))
    -- convert to domain types, except of BigNum
    multiasset'' :: Map T.CurrencySymbol (Map T.TokenName BigNum) <-
      multiasset' #
        -- convert transporting out Maybes
        ( traverse
            ( bitraverse
                -- scripthash to currency symbol
                (scriptHashToBytes >>> unwrap >>> T.mkCurrencySymbol)
                -- nested assetname to tokenname
                (traverse (ltraverse (T.assetNameName >>> T.mkTokenName)))
            )
            >>>
              -- convert inner array
              (map >>> map >>> map) Map.fromFoldable
            >>>
              -- convert outer array
              map Map.fromFoldable
        )
    -- convert BigNum values, possibly failing
    traverse (traverse bigNumToBigInt) multiasset''
  pure
    $ T.mkValue (T.Coin coin)
    $ T.mkNonAdaAsset (fromMaybe Map.empty multiasset)

foreign import getInput :: TransactionUnspentOutput -> TransactionInput
foreign import getOutput :: TransactionUnspentOutput -> TransactionOutput
foreign import getTransactionHash :: TransactionInput -> TransactionHash
foreign import getTransactionIndex :: TransactionInput -> Int
foreign import getAddress :: TransactionOutput -> Address
foreign import getAmount :: TransactionOutput -> Value
foreign import getCoin :: Value -> BigNum
foreign import getMultiAsset :: MaybeFfiHelper -> Value -> Maybe MultiAsset
foreign import extractMultiAsset
  :: (forall (a :: Type) (b :: Type). a -> b -> a /\ b)
  -> MultiAsset
  -> Array (ScriptHash /\ Assets)

foreign import extractAssets
  :: (forall (a :: Type) (b :: Type). a -> b -> a /\ b)
  -> Assets
  -> Array (AssetName /\ BigNum)

foreign import getDataHash
  :: MaybeFfiHelper -> TransactionOutput -> Maybe DataHash

foreign import mkTransactionUnspentOutput
  :: TransactionInput -> TransactionOutput -> TransactionUnspentOutput

foreign import _newTransactionUnspentOutputFromBytes
  :: MaybeFfiHelper -> ByteArray -> Maybe TransactionUnspentOutput

newTransactionUnspentOutputFromBytes
  :: ByteArray -> Maybe TransactionUnspentOutput
newTransactionUnspentOutputFromBytes = _newTransactionUnspentOutputFromBytes
  maybeFfiHelper
