module Ctl.Internal.Deserialization.UnspentOutput
  ( convertUnspentOutput
  , mkTransactionUnspentOutput
  , convertInput
  , convertOutput
  , convertValue
  ) where

import Prelude

import Cardano.Serialization.Lib (toBytes)
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
import Data.UInt (UInt)

convertUnspentOutput
  :: TransactionUnspentOutput -> Maybe T.TransactionUnspentOutput
convertUnspentOutput tuo = do
  let
    input = convertInput $ getInput tuo
  output <- convertOutput $ getOutput tuo
  pure $ T.TransactionUnspentOutput { input, output }

convertInput :: TransactionInput -> T.TransactionInput
convertInput input =
  let
    index = getTransactionIndex input
    transactionId = T.TransactionHash $ getTransactionHash input
  in
    T.TransactionInput
      { transactionId
      , index
      }

convertOutput :: TransactionOutput -> Maybe T.TransactionOutput
convertOutput output = do
  amount <- convertValue $ getAmount output
  let
    address = getAddress output
    mbDataHash =
      getDataHash maybeFfiHelper output <#> toBytes >>> T.DataHash
    mbDatum = getPlutusData maybeFfiHelper output
  datum <- case mbDatum, mbDataHash of
    Just _, Just _ -> Nothing -- impossible, so it's better to fail
    Just datumValue, Nothing -> pure $ OutputDatum $ wrap $
      convertPlutusData datumValue
    Nothing, Just datumHash -> pure $ OutputDatumHash datumHash
    Nothing, Nothing -> pure NoOutputDatum
  let
    scriptRef = getScriptRef maybeFfiHelper output <#> convertScriptRef
  pure $ T.TransactionOutput
    { address, amount, datum, scriptRef }

convertScriptRef :: ScriptRef -> T.ScriptRef
convertScriptRef = withScriptRef
  (convertNativeScript >>> T.NativeScriptRef)
  (convertPlutusScript >>> T.PlutusScriptRef)

convertValue :: Value -> Maybe T.Value
convertValue value = do
  let coin = BigNum.toBigInt $ getCoin value
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
    pure $ map (map BigNum.toBigInt) multiasset''
  pure
    $ T.mkValue (T.Coin coin)
    $ T.mkNonAdaAsset (fromMaybe Map.empty multiasset)

foreign import getInput :: TransactionUnspentOutput -> TransactionInput
foreign import getOutput :: TransactionUnspentOutput -> TransactionOutput
foreign import getTransactionHash :: TransactionInput -> TransactionHash
foreign import getTransactionIndex :: TransactionInput -> UInt
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
