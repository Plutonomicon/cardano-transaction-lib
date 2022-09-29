module Ctl.Internal.Deserialization.UnspentOutput
  ( convertUnspentOutput
  , mkTransactionUnspentOutput
  , newTransactionUnspentOutputFromBytes
  , convertInput
  , convertOutput
  , convertValue
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(PlutusScriptRef, NativeScriptRef)
  ) as T
import Ctl.Internal.Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  ) as T
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) as T
import Ctl.Internal.Cardano.Types.Value
  ( Coin(Coin)
  , CurrencySymbol
  , Value
  , mkCurrencySymbol
  , mkNonAdaAsset
  , mkValue
  ) as T
import Ctl.Internal.Deserialization.NativeScript (convertNativeScript)
import Ctl.Internal.Deserialization.PlutusData (convertPlutusData)
import Ctl.Internal.Deserialization.WitnessSet (convertPlutusScript)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Serialization.Hash (ScriptHash, scriptHashToBytes)
import Ctl.Internal.Serialization.ToBytes (toBytes)
import Ctl.Internal.Serialization.Types
  ( AssetName
  , Assets
  , DataHash
  , MultiAsset
  , NativeScript
  , PlutusData
  , PlutusScript
  , ScriptRef
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  , TransactionUnspentOutput
  , Value
  )
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.BigNum (toBigInt) as BigNum
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  )
import Ctl.Internal.Types.TokenName (TokenName, assetNameName, mkTokenName) as T
import Ctl.Internal.Types.Transaction
  ( DataHash(DataHash)
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  ) as T
import Data.Bitraversable (bitraverse, ltraverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Data.UInt as UInt

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
        (getTransactionHash input)
    , index
    }

convertOutput :: TransactionOutput -> Maybe T.TransactionOutput
convertOutput output = do
  amount <- convertValue $ getAmount output
  let
    address = getAddress output
    mbDataHash =
      getDataHash maybeFfiHelper output <#>
        toBytes >>> T.DataHash
    mbDatum = getPlutusData maybeFfiHelper output
  datum <- case mbDatum, mbDataHash of
    Just _, Just _ -> Nothing -- impossible, so it's better to fail
    Just datumValue, Nothing -> OutputDatum <<< wrap <$> convertPlutusData
      datumValue
    Nothing, Just datumHash -> pure $ OutputDatumHash datumHash
    Nothing, Nothing -> pure NoOutputDatum
  scriptRef <- getScriptRef maybeFfiHelper output # traverse convertScriptRef
  pure $ T.TransactionOutput
    { address, amount, datum, scriptRef }

convertScriptRef :: ScriptRef -> Maybe T.ScriptRef
convertScriptRef = withScriptRef
  (convertNativeScript >>> map T.NativeScriptRef)
  (convertPlutusScript >>> map T.PlutusScriptRef)

convertValue :: Value -> Maybe T.Value
convertValue value = do
  coin <- BigNum.toBigInt $ getCoin value
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
    traverse (traverse BigNum.toBigInt) multiasset''
  pure
    $ T.mkValue (T.Coin coin)
    $ T.mkNonAdaAsset (fromMaybe Map.empty multiasset)

foreign import getInput :: TransactionUnspentOutput -> TransactionInput
foreign import getOutput :: TransactionUnspentOutput -> TransactionOutput
foreign import getTransactionHash :: TransactionInput -> TransactionHash
foreign import getTransactionIndex :: TransactionInput -> Int
foreign import getAddress :: TransactionOutput -> Address
foreign import getPlutusData
  :: MaybeFfiHelper -> TransactionOutput -> Maybe PlutusData

foreign import getScriptRef
  :: MaybeFfiHelper -> TransactionOutput -> Maybe ScriptRef

foreign import withScriptRef
  :: forall (a :: Type)
   . (NativeScript -> a)
  -> (PlutusScript -> a)
  -> ScriptRef
  -> a

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
