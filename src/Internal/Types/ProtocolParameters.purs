module Ctl.Internal.Types.ProtocolParameters
  ( ProtocolParameters(ProtocolParameters)
  , CostModelV1
  , CostModelV2
  , convertPlutusV1CostModel
  , convertPlutusV2CostModel
  ) where

import Prelude

import Cardano.Types.Coin (Coin)
import Cardano.Types.CostModel (CostModel)
import Cardano.Types.Epoch (Epoch)
import Cardano.Types.ExUnitPrices (ExUnitPrices)
import Cardano.Types.ExUnits (ExUnits)
import Cardano.Types.Int as Cardano
import Cardano.Types.Language (Language)
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
  , txFeeFixed :: UInt
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
