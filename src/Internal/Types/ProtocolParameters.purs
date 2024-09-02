module Ctl.Internal.Types.ProtocolParameters
  ( ProtocolParameters(ProtocolParameters)
  , CostModelV1
  , CostModelV2
  , CostModelV3
  , convertPlutusV1CostModel
  , convertPlutusV2CostModel
  , convertPlutusV3CostModel
  ) where

import Prelude

import Cardano.Types
  ( Coin
  , CostModel
  , Epoch
  , ExUnitPrices
  , ExUnits
  , Language
  )
import Cardano.Types.Int as Cardano
import Ctl.Internal.Types.Rational (Rational)
import Data.Array (reverse)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)
import Heterogeneous.Folding (class HFoldl, hfoldl)

-- Based on `Cardano.Api.ProtocolParameters.ProtocolParameters` from
-- `cardano-api`.
newtype ProtocolParameters = ProtocolParameters
  { protocolVersion :: UInt /\ UInt
  , decentralization :: Rational
  , maxBlockHeaderSize :: UInt
  , maxBlockBodySize :: UInt
  , maxTxSize :: UInt
  , txFeeFixed :: Coin
  , txFeePerByte :: UInt
  , stakeAddressDeposit :: Coin
  , stakePoolDeposit :: Coin
  , minPoolCost :: Coin
  , poolRetireMaxEpoch :: Epoch
  , stakePoolTargetNum :: UInt
  , poolPledgeInfluence :: Rational
  , monetaryExpansion :: Rational
  , treasuryCut :: Rational
  , coinsPerUtxoByte :: Coin
  , costModels :: Map Language CostModel
  , prices :: ExUnitPrices
  , maxTxExUnits :: ExUnits
  , maxBlockExUnits :: ExUnits
  , maxValueSize :: UInt
  , collateralPercent :: UInt
  , maxCollateralInputs :: UInt
  , govActionDeposit :: Coin
  , drepDeposit :: Coin
  , refScriptCoinsPerByte :: Rational
  }

derive instance Newtype ProtocolParameters _
derive instance Generic ProtocolParameters _
derive instance Eq ProtocolParameters

instance Show ProtocolParameters where
  show = genericShow

-- | A type that represents a JSON-encoded Costmodel in format used by Ogmios
type CostModelV1 =
  ( "addInteger-cpu-arguments-intercept" :: Cardano.Int
  , "addInteger-cpu-arguments-slope" :: Cardano.Int
  , "addInteger-memory-arguments-intercept" :: Cardano.Int
  , "addInteger-memory-arguments-slope" :: Cardano.Int
  , "appendByteString-cpu-arguments-intercept" :: Cardano.Int
  , "appendByteString-cpu-arguments-slope" :: Cardano.Int
  , "appendByteString-memory-arguments-intercept" :: Cardano.Int
  , "appendByteString-memory-arguments-slope" :: Cardano.Int
  , "appendString-cpu-arguments-intercept" :: Cardano.Int
  , "appendString-cpu-arguments-slope" :: Cardano.Int
  , "appendString-memory-arguments-intercept" :: Cardano.Int
  , "appendString-memory-arguments-slope" :: Cardano.Int
  , "bData-cpu-arguments" :: Cardano.Int
  , "bData-memory-arguments" :: Cardano.Int
  , "blake2b_256-cpu-arguments-intercept" :: Cardano.Int
  , "blake2b_256-cpu-arguments-slope" :: Cardano.Int
  , "blake2b_256-memory-arguments" :: Cardano.Int
  , "cekApplyCost-exBudgetCPU" :: Cardano.Int
  , "cekApplyCost-exBudgetMemory" :: Cardano.Int
  , "cekBuiltinCost-exBudgetCPU" :: Cardano.Int
  , "cekBuiltinCost-exBudgetMemory" :: Cardano.Int
  , "cekConstCost-exBudgetCPU" :: Cardano.Int
  , "cekConstCost-exBudgetMemory" :: Cardano.Int
  , "cekDelayCost-exBudgetCPU" :: Cardano.Int
  , "cekDelayCost-exBudgetMemory" :: Cardano.Int
  , "cekForceCost-exBudgetCPU" :: Cardano.Int
  , "cekForceCost-exBudgetMemory" :: Cardano.Int
  , "cekLamCost-exBudgetCPU" :: Cardano.Int
  , "cekLamCost-exBudgetMemory" :: Cardano.Int
  , "cekStartupCost-exBudgetCPU" :: Cardano.Int
  , "cekStartupCost-exBudgetMemory" :: Cardano.Int
  , "cekVarCost-exBudgetCPU" :: Cardano.Int
  , "cekVarCost-exBudgetMemory" :: Cardano.Int
  , "chooseData-cpu-arguments" :: Cardano.Int
  , "chooseData-memory-arguments" :: Cardano.Int
  , "chooseList-cpu-arguments" :: Cardano.Int
  , "chooseList-memory-arguments" :: Cardano.Int
  , "chooseUnit-cpu-arguments" :: Cardano.Int
  , "chooseUnit-memory-arguments" :: Cardano.Int
  , "consByteString-cpu-arguments-intercept" :: Cardano.Int
  , "consByteString-cpu-arguments-slope" :: Cardano.Int
  , "consByteString-memory-arguments-intercept" :: Cardano.Int
  , "consByteString-memory-arguments-slope" :: Cardano.Int
  , "constrData-cpu-arguments" :: Cardano.Int
  , "constrData-memory-arguments" :: Cardano.Int
  , "decodeUtf8-cpu-arguments-intercept" :: Cardano.Int
  , "decodeUtf8-cpu-arguments-slope" :: Cardano.Int
  , "decodeUtf8-memory-arguments-intercept" :: Cardano.Int
  , "decodeUtf8-memory-arguments-slope" :: Cardano.Int
  , "divideInteger-cpu-arguments-constant" :: Cardano.Int
  , "divideInteger-cpu-arguments-model-arguments-intercept" :: Cardano.Int
  , "divideInteger-cpu-arguments-model-arguments-slope" :: Cardano.Int
  , "divideInteger-memory-arguments-intercept" :: Cardano.Int
  , "divideInteger-memory-arguments-minimum" :: Cardano.Int
  , "divideInteger-memory-arguments-slope" :: Cardano.Int
  , "encodeUtf8-cpu-arguments-intercept" :: Cardano.Int
  , "encodeUtf8-cpu-arguments-slope" :: Cardano.Int
  , "encodeUtf8-memory-arguments-intercept" :: Cardano.Int
  , "encodeUtf8-memory-arguments-slope" :: Cardano.Int
  , "equalsByteString-cpu-arguments-constant" :: Cardano.Int
  , "equalsByteString-cpu-arguments-intercept" :: Cardano.Int
  , "equalsByteString-cpu-arguments-slope" :: Cardano.Int
  , "equalsByteString-memory-arguments" :: Cardano.Int
  , "equalsData-cpu-arguments-intercept" :: Cardano.Int
  , "equalsData-cpu-arguments-slope" :: Cardano.Int
  , "equalsData-memory-arguments" :: Cardano.Int
  , "equalsInteger-cpu-arguments-intercept" :: Cardano.Int
  , "equalsInteger-cpu-arguments-slope" :: Cardano.Int
  , "equalsInteger-memory-arguments" :: Cardano.Int
  , "equalsString-cpu-arguments-constant" :: Cardano.Int
  , "equalsString-cpu-arguments-intercept" :: Cardano.Int
  , "equalsString-cpu-arguments-slope" :: Cardano.Int
  , "equalsString-memory-arguments" :: Cardano.Int
  , "fstPair-cpu-arguments" :: Cardano.Int
  , "fstPair-memory-arguments" :: Cardano.Int
  , "headList-cpu-arguments" :: Cardano.Int
  , "headList-memory-arguments" :: Cardano.Int
  , "iData-cpu-arguments" :: Cardano.Int
  , "iData-memory-arguments" :: Cardano.Int
  , "ifThenElse-cpu-arguments" :: Cardano.Int
  , "ifThenElse-memory-arguments" :: Cardano.Int
  , "indexByteString-cpu-arguments" :: Cardano.Int
  , "indexByteString-memory-arguments" :: Cardano.Int
  , "lengthOfByteString-cpu-arguments" :: Cardano.Int
  , "lengthOfByteString-memory-arguments" :: Cardano.Int
  , "lessThanByteString-cpu-arguments-intercept" :: Cardano.Int
  , "lessThanByteString-cpu-arguments-slope" :: Cardano.Int
  , "lessThanByteString-memory-arguments" :: Cardano.Int
  , "lessThanEqualsByteString-cpu-arguments-intercept" :: Cardano.Int
  , "lessThanEqualsByteString-cpu-arguments-slope" :: Cardano.Int
  , "lessThanEqualsByteString-memory-arguments" :: Cardano.Int
  , "lessThanEqualsInteger-cpu-arguments-intercept" :: Cardano.Int
  , "lessThanEqualsInteger-cpu-arguments-slope" :: Cardano.Int
  , "lessThanEqualsInteger-memory-arguments" :: Cardano.Int
  , "lessThanInteger-cpu-arguments-intercept" :: Cardano.Int
  , "lessThanInteger-cpu-arguments-slope" :: Cardano.Int
  , "lessThanInteger-memory-arguments" :: Cardano.Int
  , "listData-cpu-arguments" :: Cardano.Int
  , "listData-memory-arguments" :: Cardano.Int
  , "mapData-cpu-arguments" :: Cardano.Int
  , "mapData-memory-arguments" :: Cardano.Int
  , "mkCons-cpu-arguments" :: Cardano.Int
  , "mkCons-memory-arguments" :: Cardano.Int
  , "mkNilData-cpu-arguments" :: Cardano.Int
  , "mkNilData-memory-arguments" :: Cardano.Int
  , "mkNilPairData-cpu-arguments" :: Cardano.Int
  , "mkNilPairData-memory-arguments" :: Cardano.Int
  , "mkPairData-cpu-arguments" :: Cardano.Int
  , "mkPairData-memory-arguments" :: Cardano.Int
  , "modInteger-cpu-arguments-constant" :: Cardano.Int
  , "modInteger-cpu-arguments-model-arguments-intercept" :: Cardano.Int
  , "modInteger-cpu-arguments-model-arguments-slope" :: Cardano.Int
  , "modInteger-memory-arguments-intercept" :: Cardano.Int
  , "modInteger-memory-arguments-minimum" :: Cardano.Int
  , "modInteger-memory-arguments-slope" :: Cardano.Int
  , "multiplyInteger-cpu-arguments-intercept" :: Cardano.Int
  , "multiplyInteger-cpu-arguments-slope" :: Cardano.Int
  , "multiplyInteger-memory-arguments-intercept" :: Cardano.Int
  , "multiplyInteger-memory-arguments-slope" :: Cardano.Int
  , "nullList-cpu-arguments" :: Cardano.Int
  , "nullList-memory-arguments" :: Cardano.Int
  , "quotientInteger-cpu-arguments-constant" :: Cardano.Int
  , "quotientInteger-cpu-arguments-model-arguments-intercept" :: Cardano.Int
  , "quotientInteger-cpu-arguments-model-arguments-slope" :: Cardano.Int
  , "quotientInteger-memory-arguments-intercept" :: Cardano.Int
  , "quotientInteger-memory-arguments-minimum" :: Cardano.Int
  , "quotientInteger-memory-arguments-slope" :: Cardano.Int
  , "remainderInteger-cpu-arguments-constant" :: Cardano.Int
  , "remainderInteger-cpu-arguments-model-arguments-intercept" :: Cardano.Int
  , "remainderInteger-cpu-arguments-model-arguments-slope" :: Cardano.Int
  , "remainderInteger-memory-arguments-intercept" :: Cardano.Int
  , "remainderInteger-memory-arguments-minimum" :: Cardano.Int
  , "remainderInteger-memory-arguments-slope" :: Cardano.Int
  , "sha2_256-cpu-arguments-intercept" :: Cardano.Int
  , "sha2_256-cpu-arguments-slope" :: Cardano.Int
  , "sha2_256-memory-arguments" :: Cardano.Int
  , "sha3_256-cpu-arguments-intercept" :: Cardano.Int
  , "sha3_256-cpu-arguments-slope" :: Cardano.Int
  , "sha3_256-memory-arguments" :: Cardano.Int
  , "sliceByteString-cpu-arguments-intercept" :: Cardano.Int
  , "sliceByteString-cpu-arguments-slope" :: Cardano.Int
  , "sliceByteString-memory-arguments-intercept" :: Cardano.Int
  , "sliceByteString-memory-arguments-slope" :: Cardano.Int
  , "sndPair-cpu-arguments" :: Cardano.Int
  , "sndPair-memory-arguments" :: Cardano.Int
  , "subtractInteger-cpu-arguments-intercept" :: Cardano.Int
  , "subtractInteger-cpu-arguments-slope" :: Cardano.Int
  , "subtractInteger-memory-arguments-intercept" :: Cardano.Int
  , "subtractInteger-memory-arguments-slope" :: Cardano.Int
  , "tailList-cpu-arguments" :: Cardano.Int
  , "tailList-memory-arguments" :: Cardano.Int
  , "trace-cpu-arguments" :: Cardano.Int
  , "trace-memory-arguments" :: Cardano.Int
  , "unBData-cpu-arguments" :: Cardano.Int
  , "unBData-memory-arguments" :: Cardano.Int
  , "unConstrData-cpu-arguments" :: Cardano.Int
  , "unConstrData-memory-arguments" :: Cardano.Int
  , "unIData-cpu-arguments" :: Cardano.Int
  , "unIData-memory-arguments" :: Cardano.Int
  , "unListData-cpu-arguments" :: Cardano.Int
  , "unListData-memory-arguments" :: Cardano.Int
  , "unMapData-cpu-arguments" :: Cardano.Int
  , "unMapData-memory-arguments" :: Cardano.Int
  , "verifyEd25519Signature-cpu-arguments-intercept" :: Cardano.Int
  , "verifyEd25519Signature-cpu-arguments-slope" :: Cardano.Int
  , "verifyEd25519Signature-memory-arguments" :: Cardano.Int
  )

type CostModelV2 =
  ( "serialiseData-cpu-arguments-intercept" :: Cardano.Int
  , "serialiseData-cpu-arguments-slope" :: Cardano.Int
  , "serialiseData-memory-arguments-intercept" :: Cardano.Int
  , "serialiseData-memory-arguments-slope" :: Cardano.Int
  , "verifyEcdsaSecp256k1Signature-cpu-arguments" :: Cardano.Int
  , "verifyEcdsaSecp256k1Signature-memory-arguments" :: Cardano.Int
  , "verifySchnorrSecp256k1Signature-cpu-arguments-intercept" :: Cardano.Int
  , "verifySchnorrSecp256k1Signature-cpu-arguments-slope" :: Cardano.Int
  , "verifySchnorrSecp256k1Signature-memory-arguments" :: Cardano.Int
  | CostModelV1
  )

type CostModelV3 =
  (
  "divideInteger-cpu-arguments-model-arguments-c00" :: Cardano.Int,
  "divideInteger-cpu-arguments-model-arguments-c01" :: Cardano.Int,
  "divideInteger-cpu-arguments-model-arguments-c02" :: Cardano.Int,
  "divideInteger-cpu-arguments-model-arguments-c10" :: Cardano.Int,
  "divideInteger-cpu-arguments-model-arguments-c11" :: Cardano.Int,
  "divideInteger-cpu-arguments-model-arguments-c20" :: Cardano.Int,
  "divideInteger-cpu-arguments-model-arguments-minimum" :: Cardano.Int,
  "divideInteger-memory-arguments-intercept" :: Cardano.Int,
  "divideInteger-memory-arguments-minimum" :: Cardano.Int,
  "divideInteger-memory-arguments-slope" :: Cardano.Int,
  "encodeUtf8-cpu-arguments-intercept" :: Cardano.Int,
  "encodeUtf8-cpu-arguments-slope" :: Cardano.Int,
  "encodeUtf8-memory-arguments-intercept" :: Cardano.Int,
  "encodeUtf8-memory-arguments-slope" :: Cardano.Int,
  "equalsByteString-cpu-arguments-constant" :: Cardano.Int,
  "equalsByteString-cpu-arguments-intercept" :: Cardano.Int,
  "equalsByteString-cpu-arguments-slope" :: Cardano.Int,
  "equalsByteString-memory-arguments" :: Cardano.Int,
  "equalsData-cpu-arguments-intercept" :: Cardano.Int,
  "equalsData-cpu-arguments-slope" :: Cardano.Int,
  "equalsData-memory-arguments" :: Cardano.Int,
  "equalsInteger-cpu-arguments-intercept" :: Cardano.Int,
  "equalsInteger-cpu-arguments-slope" :: Cardano.Int,
  "equalsInteger-memory-arguments" :: Cardano.Int,
  "equalsString-cpu-arguments-constant" :: Cardano.Int,
  "equalsString-cpu-arguments-intercept" :: Cardano.Int,
  "equalsString-cpu-arguments-slope" :: Cardano.Int,
  "equalsString-memory-arguments" :: Cardano.Int,
  "fstPair-cpu-arguments" :: Cardano.Int,
  "fstPair-memory-arguments" :: Cardano.Int,
  "headList-cpu-arguments" :: Cardano.Int,
  "headList-memory-arguments" :: Cardano.Int,
  "iData-cpu-arguments" :: Cardano.Int,
  "iData-memory-arguments" :: Cardano.Int,
  "ifThenElse-cpu-arguments" :: Cardano.Int,
  "ifThenElse-memory-arguments" :: Cardano.Int,
  "indexByteString-cpu-arguments" :: Cardano.Int,
  "indexByteString-memory-arguments" :: Cardano.Int,
  "lengthOfByteString-cpu-arguments" :: Cardano.Int,
  "lengthOfByteString-memory-arguments" :: Cardano.Int,
  "lessThanByteString-cpu-arguments-intercept" :: Cardano.Int,
  "lessThanByteString-cpu-arguments-slope" :: Cardano.Int,
  "lessThanByteString-memory-arguments" :: Cardano.Int,
  "lessThanEqualsByteString-cpu-arguments-intercept" :: Cardano.Int,
  "lessThanEqualsByteString-cpu-arguments-slope" :: Cardano.Int,
  "lessThanEqualsByteString-memory-arguments" :: Cardano.Int,
  "lessThanEqualsInteger-cpu-arguments-intercept" :: Cardano.Int,
  "lessThanEqualsInteger-cpu-arguments-slope" :: Cardano.Int,
  "lessThanEqualsInteger-memory-arguments" :: Cardano.Int,
  "lessThanInteger-cpu-arguments-intercept" :: Cardano.Int,
  "lessThanInteger-cpu-arguments-slope" :: Cardano.Int,
  "lessThanInteger-memory-arguments" :: Cardano.Int,
  "listData-cpu-arguments" :: Cardano.Int,
  "listData-memory-arguments" :: Cardano.Int,
  "mapData-cpu-arguments" :: Cardano.Int,
  "mapData-memory-arguments" :: Cardano.Int,
  "mkCons-cpu-arguments" :: Cardano.Int,
  "mkCons-memory-arguments" :: Cardano.Int,
  "mkNilData-cpu-arguments" :: Cardano.Int,
  "mkNilData-memory-arguments" :: Cardano.Int,
  "mkNilPairData-cpu-arguments" :: Cardano.Int,
  "mkNilPairData-memory-arguments" :: Cardano.Int,
  "mkPairData-cpu-arguments" :: Cardano.Int,
  "mkPairData-memory-arguments" :: Cardano.Int,
  "modInteger-cpu-arguments-constant" :: Cardano.Int,
  "modInteger-cpu-arguments-model-arguments-c00" :: Cardano.Int,
  "modInteger-cpu-arguments-model-arguments-c01" :: Cardano.Int,
  "modInteger-cpu-arguments-model-arguments-c02" :: Cardano.Int,
  "modInteger-cpu-arguments-model-arguments-c10" :: Cardano.Int,
  "modInteger-cpu-arguments-model-arguments-c11" :: Cardano.Int,
  "modInteger-cpu-arguments-model-arguments-c20" :: Cardano.Int,
  "modInteger-cpu-arguments-model-arguments-minimum" :: Cardano.Int,
  "modInteger-memory-arguments-intercept" :: Cardano.Int,
  "modInteger-memory-arguments-slope" :: Cardano.Int,
  "multiplyInteger-cpu-arguments-intercept" :: Cardano.Int,
  "multiplyInteger-cpu-arguments-slope" :: Cardano.Int,
  "multiplyInteger-memory-arguments-intercept" :: Cardano.Int,
  "multiplyInteger-memory-arguments-slope" :: Cardano.Int,
  "nullList-cpu-arguments" :: Cardano.Int,
  "nullList-memory-arguments" :: Cardano.Int,
  "quotientInteger-cpu-arguments-constant" :: Cardano.Int,
  "quotientInteger-cpu-arguments-model-arguments-c00" :: Cardano.Int,
  "quotientInteger-cpu-arguments-model-arguments-c01" :: Cardano.Int,
  "quotientInteger-cpu-arguments-model-arguments-c02" :: Cardano.Int,
  "quotientInteger-cpu-arguments-model-arguments-c10" :: Cardano.Int,
  "quotientInteger-cpu-arguments-model-arguments-c11" :: Cardano.Int,
  "quotientInteger-cpu-arguments-model-arguments-c20" :: Cardano.Int,
  "quotientInteger-cpu-arguments-model-arguments-minimum" :: Cardano.Int,
  "quotientInteger-memory-arguments-intercept" :: Cardano.Int,
  "quotientInteger-memory-arguments-slope" :: Cardano.Int,
  "remainderInteger-cpu-arguments-constant" :: Cardano.Int,
  "remainderInteger-cpu-arguments-model-arguments-c00" :: Cardano.Int,
  "remainderInteger-cpu-arguments-model-arguments-c01" :: Cardano.Int,
  "remainderInteger-cpu-arguments-model-arguments-c02" :: Cardano.Int,
  "remainderInteger-cpu-arguments-model-arguments-c10" :: Cardano.Int,
  "remainderInteger-cpu-arguments-model-arguments-c11" :: Cardano.Int,
  "remainderInteger-cpu-arguments-model-arguments-c20" :: Cardano.Int,
  "remainderInteger-cpu-arguments-model-arguments-minimum" :: Cardano.Int,
  "remainderInteger-memory-arguments-intercept" :: Cardano.Int,
  "remainderInteger-memory-arguments-minimum" :: Cardano.Int,
  "remainderInteger-memory-arguments-slope" :: Cardano.Int,
  "serialiseData-cpu-arguments-intercept" :: Cardano.Int,
  "serialiseData-cpu-arguments-slope" :: Cardano.Int,
  "serialiseData-memory-arguments-intercept" :: Cardano.Int,
  "serialiseData-memory-arguments-slope" :: Cardano.Int,
  "sha2_256-cpu-arguments-intercept" :: Cardano.Int,
  "sha2_256-cpu-arguments-slope" :: Cardano.Int,
  "sha2_256-memory-arguments" :: Cardano.Int,
  "sha3_256-cpu-arguments-intercept" :: Cardano.Int,
  "sha3_256-cpu-arguments-slope" :: Cardano.Int,
  "sha3_256-memory-arguments" :: Cardano.Int,
  "sliceByteString-cpu-arguments-intercept" :: Cardano.Int,
  "sliceByteString-cpu-arguments-slope" :: Cardano.Int,
  "sliceByteString-memory-arguments-intercept" :: Cardano.Int,
  "sliceByteString-memory-arguments-slope" :: Cardano.Int,
  "sndPair-cpu-arguments" :: Cardano.Int,
  "sndPair-memory-arguments" :: Cardano.Int,
  "subtractInteger-cpu-arguments-intercept" :: Cardano.Int,
  "subtractInteger-cpu-arguments-slope" :: Cardano.Int,
  "subtractInteger-memory-arguments-intercept" :: Cardano.Int,
  "subtractInteger-memory-arguments-slope" :: Cardano.Int,
  "tailList-cpu-arguments" :: Cardano.Int,
  "tailList-memory-arguments" :: Cardano.Int,
  "trace-cpu-arguments" :: Cardano.Int,
  "trace-memory-arguments" :: Cardano.Int,
  "unBData-cpu-arguments" :: Cardano.Int,
  "unBData-memory-arguments" :: Cardano.Int,
  "unConstrData-cpu-arguments" :: Cardano.Int,
  "unConstrData-memory-arguments" :: Cardano.Int,
  "unIData-cpu-arguments" :: Cardano.Int,
  "unIData-memory-arguments" :: Cardano.Int,
  "unListData-cpu-arguments" :: Cardano.Int,
  "unListData-memory-arguments" :: Cardano.Int,
  "unMapData-cpu-arguments" :: Cardano.Int,
  "unMapData-memory-arguments" :: Cardano.Int,
  "verifyEcdsaSecp256k1Signature-cpu-arguments" :: Cardano.Int,
  "verifyEcdsaSecp256k1Signature-memory-arguments" :: Cardano.Int,
  "verifyEd25519Signature-cpu-arguments-intercept" :: Cardano.Int,
  "verifyEd25519Signature-cpu-arguments-slope" :: Cardano.Int,
  "verifyEd25519Signature-memory-arguments" :: Cardano.Int,
  "verifySchnorrSecp256k1Signature-cpu-arguments-intercept" :: Cardano.Int,
  "verifySchnorrSecp256k1Signature-cpu-arguments-slope" :: Cardano.Int,
  "verifySchnorrSecp256k1Signature-memory-arguments" :: Cardano.Int,
  "cekConstrCost-exBudgetCPU" :: Cardano.Int,
  "cekConstrCost-exBudgetMemory" :: Cardano.Int,
  "cekCaseCost-exBudgetCPU" :: Cardano.Int,
  "cekCaseCost-exBudgetMemory" :: Cardano.Int,
  "bls12_381_G1_add-cpu-arguments" :: Cardano.Int,
  "bls12_381_G1_add-memory-arguments" :: Cardano.Int,
  "bls12_381_G1_compress-cpu-arguments" :: Cardano.Int,
  "bls12_381_G1_compress-memory-arguments" :: Cardano.Int,
  "bls12_381_G1_equal-cpu-arguments" :: Cardano.Int,
  "bls12_381_G1_equal-memory-arguments" :: Cardano.Int,
  "bls12_381_G1_hashToGroup-cpu-arguments-intercept" :: Cardano.Int,
  "bls12_381_G1_hashToGroup-cpu-arguments-slope" :: Cardano.Int,
  "bls12_381_G1_hashToGroup-memory-arguments" :: Cardano.Int,
  "bls12_381_G1_neg-cpu-arguments" :: Cardano.Int,
  "bls12_381_G1_neg-memory-arguments" :: Cardano.Int,
  "bls12_381_G1_scalarMul-cpu-arguments-intercept" :: Cardano.Int,
  "bls12_381_G1_scalarMul-cpu-arguments-slope" :: Cardano.Int,
  "bls12_381_G1_scalarMul-memory-arguments" :: Cardano.Int,
  "bls12_381_G1_uncompress-cpu-arguments" :: Cardano.Int,
  "bls12_381_G1_uncompress-memory-arguments" :: Cardano.Int,
  "bls12_381_G2_add-cpu-arguments" :: Cardano.Int,
  "bls12_381_G2_add-memory-arguments" :: Cardano.Int,
  "bls12_381_G2_compress-cpu-arguments" :: Cardano.Int,
  "bls12_381_G2_compress-memory-arguments" :: Cardano.Int,
  "bls12_381_G2_equal-cpu-arguments" :: Cardano.Int,
  "bls12_381_G2_equal-memory-arguments" :: Cardano.Int,
  "bls12_381_G2_hashToGroup-cpu-arguments-intercept" :: Cardano.Int,
  "bls12_381_G2_hashToGroup-cpu-arguments-slope" :: Cardano.Int,
  "bls12_381_G2_hashToGroup-memory-arguments" :: Cardano.Int,
  "bls12_381_G2_neg-cpu-arguments" :: Cardano.Int,
  "bls12_381_G2_neg-memory-arguments" :: Cardano.Int,
  "bls12_381_G2_scalarMul-cpu-arguments-intercept" :: Cardano.Int,
  "bls12_381_G2_scalarMul-cpu-arguments-slope" :: Cardano.Int,
  "bls12_381_G2_scalarMul-memory-arguments" :: Cardano.Int,
  "bls12_381_G2_uncompress-cpu-arguments" :: Cardano.Int,
  "bls12_381_G2_uncompress-memory-arguments" :: Cardano.Int,
  "bls12_381_finalVerify-cpu-arguments" :: Cardano.Int,
  "bls12_381_finalVerify-memory-arguments" :: Cardano.Int,
  "bls12_381_millerLoop-cpu-arguments" :: Cardano.Int,
  "bls12_381_millerLoop-memory-arguments" :: Cardano.Int,
  "bls12_381_mulMlResult-cpu-arguments" :: Cardano.Int,
  "bls12_381_mulMlResult-memory-arguments" :: Cardano.Int,
  "keccak_256-cpu-arguments-intercept" :: Cardano.Int,
  "keccak_256-cpu-arguments-slope" :: Cardano.Int,
  "keccak_256-memory-arguments" :: Cardano.Int,
  "blake2b_224-cpu-arguments-intercept" :: Cardano.Int,
  "blake2b_224-cpu-arguments-slope" :: Cardano.Int,
  "blake2b_224-memory-arguments" :: Cardano.Int,
  "integerToByteString-cpu-arguments-c0" :: Cardano.Int,
  "integerToByteString-cpu-arguments-c1" :: Cardano.Int,
  "integerToByteString-cpu-arguments-c2" :: Cardano.Int,
  "integerToByteString-memory-arguments-intercept" :: Cardano.Int,
  "integerToByteString-memory-arguments-slope" :: Cardano.Int,
  "byteStringToInteger-cpu-arguments-c0" :: Cardano.Int,
  "byteStringToInteger-cpu-arguments-c1" :: Cardano.Int,
  "byteStringToInteger-cpu-arguments-c2" :: Cardano.Int,
  "byteStringToInteger-memory-arguments-intercept" :: Cardano.Int,
  "byteStringToInteger-memory-arguments-slope" :: Cardano.Int
  )


-- This assumes that cost models are stored in lexicographical order
convertCostModel
  :: forall costModel
   . HFoldl (List Cardano.Int -> Cardano.Int -> List Cardano.Int)
       (List Cardano.Int)
       costModel
       (List Cardano.Int)
  => costModel
  -> CostModel
convertCostModel model = wrap $ reverse $ List.toUnfoldable $ hfoldl
  ( (\xs x -> x List.: xs)
      :: List Cardano.Int -> Cardano.Int -> List Cardano.Int
  )
  (mempty :: List Cardano.Int)
  model

-- Specialized conversions to only perform the type level traversals once

convertPlutusV1CostModel :: Record CostModelV1 -> CostModel
convertPlutusV1CostModel = convertCostModel

convertPlutusV2CostModel :: Record CostModelV2 -> CostModel
convertPlutusV2CostModel = convertCostModel

convertPlutusV3CostModel :: Record CostModelV3 -> CostModel
convertPlutusV3CostModel = convertCostModel
