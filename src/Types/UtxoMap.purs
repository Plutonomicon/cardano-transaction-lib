module Cardano.Types.UtxoMap where

import Prelude

import Cardano.Serialization.Lib (toBytes)
import Cardano.Types.Address as Address
import Cardano.Types.AsCbor (encodeCbor)
import Cardano.Types.OutputDatum (OutputDatum(..))
import Cardano.Types.PlutusData (pprintPlutusData)
import Cardano.Types.TransactionInput (TransactionInput(TransactionInput))
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.Value (pprintValue)
import Data.ByteArray (byteArrayToHex)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt

type UtxoMap = Map TransactionInput TransactionOutput

pprintUtxoMap :: UtxoMap -> TagSet
pprintUtxoMap utxos = TagSet.fromArray $
  Map.toUnfoldable utxos <#>
    \( TransactionInput { transactionId, index } /\
         TransactionOutput { address, amount, datum, scriptRef }
     ) ->
      let
        datumTagSets = case datum of
          Nothing -> []
          Just (OutputDatumHash datumHash) ->
            [ TagSet.fromArray
                [ "datum hash" `tag` byteArrayToHex
                    (unwrap $ encodeCbor datumHash)
                ]
            ]
          Just (OutputDatum plutusData) ->
            [ TagSet.fromArray
                [ "datum" `tagSetTag` pprintPlutusData plutusData ]
            ]
        scriptRefTagSets = case scriptRef of
          Nothing -> []
          Just ref -> [ "Script Reference" `tag` show ref ]
        outputTagSet =
          [ "amount" `tagSetTag` pprintValue amount
          , "address" `tag` Address.toBech32 address
          ]
            <> datumTagSets
            <> scriptRefTagSets
      in
        ( byteArrayToHex (toBytes $ unwrap transactionId) <> "#" <>
            UInt.toString index
        )
          `tagSetTag` TagSet.fromArray outputTagSet
