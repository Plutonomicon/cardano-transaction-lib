module Ctl.Internal.Types.CostModels
  ( CostModelV1
  , CostModelV2
  , convertPlutusV1CostModel
  , convertPlutusV2CostModel
  ) where

import Prelude

import Cardano.Types.CostModel (CostModel)
import Ctl.Internal.Types.Int as Csl
import Data.Array (reverse)
import Data.List (List)
import Data.List as List
import Data.Newtype (wrap)
import Heterogeneous.Folding (class HFoldl, hfoldl)

-- | A type that represents a JSON-encoded Costmodel in format used by Ogmios
type CostModelV1 =
  ( "addInteger-cpu-arguments-intercept" :: Csl.Int
  , "addInteger-cpu-arguments-slope" :: Csl.Int
  , "addInteger-memory-arguments-intercept" :: Csl.Int
  , "addInteger-memory-arguments-slope" :: Csl.Int
  , "appendByteString-cpu-arguments-intercept" :: Csl.Int
  , "appendByteString-cpu-arguments-slope" :: Csl.Int
  , "appendByteString-memory-arguments-intercept" :: Csl.Int
  , "appendByteString-memory-arguments-slope" :: Csl.Int
  , "appendString-cpu-arguments-intercept" :: Csl.Int
  , "appendString-cpu-arguments-slope" :: Csl.Int
  , "appendString-memory-arguments-intercept" :: Csl.Int
  , "appendString-memory-arguments-slope" :: Csl.Int
  , "bData-cpu-arguments" :: Csl.Int
  , "bData-memory-arguments" :: Csl.Int
  , "blake2b_256-cpu-arguments-intercept" :: Csl.Int
  , "blake2b_256-cpu-arguments-slope" :: Csl.Int
  , "blake2b_256-memory-arguments" :: Csl.Int
  , "cekApplyCost-exBudgetCPU" :: Csl.Int
  , "cekApplyCost-exBudgetMemory" :: Csl.Int
  , "cekBuiltinCost-exBudgetCPU" :: Csl.Int
  , "cekBuiltinCost-exBudgetMemory" :: Csl.Int
  , "cekConstCost-exBudgetCPU" :: Csl.Int
  , "cekConstCost-exBudgetMemory" :: Csl.Int
  , "cekDelayCost-exBudgetCPU" :: Csl.Int
  , "cekDelayCost-exBudgetMemory" :: Csl.Int
  , "cekForceCost-exBudgetCPU" :: Csl.Int
  , "cekForceCost-exBudgetMemory" :: Csl.Int
  , "cekLamCost-exBudgetCPU" :: Csl.Int
  , "cekLamCost-exBudgetMemory" :: Csl.Int
  , "cekStartupCost-exBudgetCPU" :: Csl.Int
  , "cekStartupCost-exBudgetMemory" :: Csl.Int
  , "cekVarCost-exBudgetCPU" :: Csl.Int
  , "cekVarCost-exBudgetMemory" :: Csl.Int
  , "chooseData-cpu-arguments" :: Csl.Int
  , "chooseData-memory-arguments" :: Csl.Int
  , "chooseList-cpu-arguments" :: Csl.Int
  , "chooseList-memory-arguments" :: Csl.Int
  , "chooseUnit-cpu-arguments" :: Csl.Int
  , "chooseUnit-memory-arguments" :: Csl.Int
  , "consByteString-cpu-arguments-intercept" :: Csl.Int
  , "consByteString-cpu-arguments-slope" :: Csl.Int
  , "consByteString-memory-arguments-intercept" :: Csl.Int
  , "consByteString-memory-arguments-slope" :: Csl.Int
  , "constrData-cpu-arguments" :: Csl.Int
  , "constrData-memory-arguments" :: Csl.Int
  , "decodeUtf8-cpu-arguments-intercept" :: Csl.Int
  , "decodeUtf8-cpu-arguments-slope" :: Csl.Int
  , "decodeUtf8-memory-arguments-intercept" :: Csl.Int
  , "decodeUtf8-memory-arguments-slope" :: Csl.Int
  , "divideInteger-cpu-arguments-constant" :: Csl.Int
  , "divideInteger-cpu-arguments-model-arguments-intercept" :: Csl.Int
  , "divideInteger-cpu-arguments-model-arguments-slope" :: Csl.Int
  , "divideInteger-memory-arguments-intercept" :: Csl.Int
  , "divideInteger-memory-arguments-minimum" :: Csl.Int
  , "divideInteger-memory-arguments-slope" :: Csl.Int
  , "encodeUtf8-cpu-arguments-intercept" :: Csl.Int
  , "encodeUtf8-cpu-arguments-slope" :: Csl.Int
  , "encodeUtf8-memory-arguments-intercept" :: Csl.Int
  , "encodeUtf8-memory-arguments-slope" :: Csl.Int
  , "equalsByteString-cpu-arguments-constant" :: Csl.Int
  , "equalsByteString-cpu-arguments-intercept" :: Csl.Int
  , "equalsByteString-cpu-arguments-slope" :: Csl.Int
  , "equalsByteString-memory-arguments" :: Csl.Int
  , "equalsData-cpu-arguments-intercept" :: Csl.Int
  , "equalsData-cpu-arguments-slope" :: Csl.Int
  , "equalsData-memory-arguments" :: Csl.Int
  , "equalsInteger-cpu-arguments-intercept" :: Csl.Int
  , "equalsInteger-cpu-arguments-slope" :: Csl.Int
  , "equalsInteger-memory-arguments" :: Csl.Int
  , "equalsString-cpu-arguments-constant" :: Csl.Int
  , "equalsString-cpu-arguments-intercept" :: Csl.Int
  , "equalsString-cpu-arguments-slope" :: Csl.Int
  , "equalsString-memory-arguments" :: Csl.Int
  , "fstPair-cpu-arguments" :: Csl.Int
  , "fstPair-memory-arguments" :: Csl.Int
  , "headList-cpu-arguments" :: Csl.Int
  , "headList-memory-arguments" :: Csl.Int
  , "iData-cpu-arguments" :: Csl.Int
  , "iData-memory-arguments" :: Csl.Int
  , "ifThenElse-cpu-arguments" :: Csl.Int
  , "ifThenElse-memory-arguments" :: Csl.Int
  , "indexByteString-cpu-arguments" :: Csl.Int
  , "indexByteString-memory-arguments" :: Csl.Int
  , "lengthOfByteString-cpu-arguments" :: Csl.Int
  , "lengthOfByteString-memory-arguments" :: Csl.Int
  , "lessThanByteString-cpu-arguments-intercept" :: Csl.Int
  , "lessThanByteString-cpu-arguments-slope" :: Csl.Int
  , "lessThanByteString-memory-arguments" :: Csl.Int
  , "lessThanEqualsByteString-cpu-arguments-intercept" :: Csl.Int
  , "lessThanEqualsByteString-cpu-arguments-slope" :: Csl.Int
  , "lessThanEqualsByteString-memory-arguments" :: Csl.Int
  , "lessThanEqualsInteger-cpu-arguments-intercept" :: Csl.Int
  , "lessThanEqualsInteger-cpu-arguments-slope" :: Csl.Int
  , "lessThanEqualsInteger-memory-arguments" :: Csl.Int
  , "lessThanInteger-cpu-arguments-intercept" :: Csl.Int
  , "lessThanInteger-cpu-arguments-slope" :: Csl.Int
  , "lessThanInteger-memory-arguments" :: Csl.Int
  , "listData-cpu-arguments" :: Csl.Int
  , "listData-memory-arguments" :: Csl.Int
  , "mapData-cpu-arguments" :: Csl.Int
  , "mapData-memory-arguments" :: Csl.Int
  , "mkCons-cpu-arguments" :: Csl.Int
  , "mkCons-memory-arguments" :: Csl.Int
  , "mkNilData-cpu-arguments" :: Csl.Int
  , "mkNilData-memory-arguments" :: Csl.Int
  , "mkNilPairData-cpu-arguments" :: Csl.Int
  , "mkNilPairData-memory-arguments" :: Csl.Int
  , "mkPairData-cpu-arguments" :: Csl.Int
  , "mkPairData-memory-arguments" :: Csl.Int
  , "modInteger-cpu-arguments-constant" :: Csl.Int
  , "modInteger-cpu-arguments-model-arguments-intercept" :: Csl.Int
  , "modInteger-cpu-arguments-model-arguments-slope" :: Csl.Int
  , "modInteger-memory-arguments-intercept" :: Csl.Int
  , "modInteger-memory-arguments-minimum" :: Csl.Int
  , "modInteger-memory-arguments-slope" :: Csl.Int
  , "multiplyInteger-cpu-arguments-intercept" :: Csl.Int
  , "multiplyInteger-cpu-arguments-slope" :: Csl.Int
  , "multiplyInteger-memory-arguments-intercept" :: Csl.Int
  , "multiplyInteger-memory-arguments-slope" :: Csl.Int
  , "nullList-cpu-arguments" :: Csl.Int
  , "nullList-memory-arguments" :: Csl.Int
  , "quotientInteger-cpu-arguments-constant" :: Csl.Int
  , "quotientInteger-cpu-arguments-model-arguments-intercept" :: Csl.Int
  , "quotientInteger-cpu-arguments-model-arguments-slope" :: Csl.Int
  , "quotientInteger-memory-arguments-intercept" :: Csl.Int
  , "quotientInteger-memory-arguments-minimum" :: Csl.Int
  , "quotientInteger-memory-arguments-slope" :: Csl.Int
  , "remainderInteger-cpu-arguments-constant" :: Csl.Int
  , "remainderInteger-cpu-arguments-model-arguments-intercept" :: Csl.Int
  , "remainderInteger-cpu-arguments-model-arguments-slope" :: Csl.Int
  , "remainderInteger-memory-arguments-intercept" :: Csl.Int
  , "remainderInteger-memory-arguments-minimum" :: Csl.Int
  , "remainderInteger-memory-arguments-slope" :: Csl.Int
  , "sha2_256-cpu-arguments-intercept" :: Csl.Int
  , "sha2_256-cpu-arguments-slope" :: Csl.Int
  , "sha2_256-memory-arguments" :: Csl.Int
  , "sha3_256-cpu-arguments-intercept" :: Csl.Int
  , "sha3_256-cpu-arguments-slope" :: Csl.Int
  , "sha3_256-memory-arguments" :: Csl.Int
  , "sliceByteString-cpu-arguments-intercept" :: Csl.Int
  , "sliceByteString-cpu-arguments-slope" :: Csl.Int
  , "sliceByteString-memory-arguments-intercept" :: Csl.Int
  , "sliceByteString-memory-arguments-slope" :: Csl.Int
  , "sndPair-cpu-arguments" :: Csl.Int
  , "sndPair-memory-arguments" :: Csl.Int
  , "subtractInteger-cpu-arguments-intercept" :: Csl.Int
  , "subtractInteger-cpu-arguments-slope" :: Csl.Int
  , "subtractInteger-memory-arguments-intercept" :: Csl.Int
  , "subtractInteger-memory-arguments-slope" :: Csl.Int
  , "tailList-cpu-arguments" :: Csl.Int
  , "tailList-memory-arguments" :: Csl.Int
  , "trace-cpu-arguments" :: Csl.Int
  , "trace-memory-arguments" :: Csl.Int
  , "unBData-cpu-arguments" :: Csl.Int
  , "unBData-memory-arguments" :: Csl.Int
  , "unConstrData-cpu-arguments" :: Csl.Int
  , "unConstrData-memory-arguments" :: Csl.Int
  , "unIData-cpu-arguments" :: Csl.Int
  , "unIData-memory-arguments" :: Csl.Int
  , "unListData-cpu-arguments" :: Csl.Int
  , "unListData-memory-arguments" :: Csl.Int
  , "unMapData-cpu-arguments" :: Csl.Int
  , "unMapData-memory-arguments" :: Csl.Int
  , "verifyEd25519Signature-cpu-arguments-intercept" :: Csl.Int
  , "verifyEd25519Signature-cpu-arguments-slope" :: Csl.Int
  , "verifyEd25519Signature-memory-arguments" :: Csl.Int
  )

type CostModelV2 =
  ( "serialiseData-cpu-arguments-intercept" :: Csl.Int
  , "serialiseData-cpu-arguments-slope" :: Csl.Int
  , "serialiseData-memory-arguments-intercept" :: Csl.Int
  , "serialiseData-memory-arguments-slope" :: Csl.Int
  , "verifyEcdsaSecp256k1Signature-cpu-arguments" :: Csl.Int
  , "verifyEcdsaSecp256k1Signature-memory-arguments" :: Csl.Int
  , "verifySchnorrSecp256k1Signature-cpu-arguments-intercept" :: Csl.Int
  , "verifySchnorrSecp256k1Signature-cpu-arguments-slope" :: Csl.Int
  , "verifySchnorrSecp256k1Signature-memory-arguments" :: Csl.Int
  | CostModelV1
  )

-- This assumes that cost models are stored in lexicographical order
convertCostModel
  :: forall costModel
   . HFoldl (List Csl.Int -> Csl.Int -> List Csl.Int) (List Csl.Int) costModel
       (List Csl.Int)
  => costModel
  -> CostModel
convertCostModel model = wrap $ reverse $ List.toUnfoldable $ hfoldl
  ((\xs x -> x List.: xs) :: List Csl.Int -> Csl.Int -> List Csl.Int)
  (mempty :: List Csl.Int)
  model

-- Specialized conversions to only perform the type level traversals once

convertPlutusV1CostModel :: Record CostModelV1 -> CostModel
convertPlutusV1CostModel = convertCostModel

convertPlutusV2CostModel :: Record CostModelV2 -> CostModel
convertPlutusV2CostModel = convertCostModel
