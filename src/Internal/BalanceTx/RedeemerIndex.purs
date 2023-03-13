module Ctl.Internal.BalanceTx.RedeemerIndex where

import Prelude

import Aeson (class EncodeAeson, encodeAeson)
import Ctl.Internal.Cardano.Types.Transaction
  ( Certificate
  , Redeemer(Redeemer)
  , Transaction(Transaction)
  , TxBody(TxBody)
  )
import Ctl.Internal.Cardano.Types.Value (currencyMPSHash)
import Ctl.Internal.Types.PlutusData (PlutusData)
import Ctl.Internal.Types.RedeemerTag (RedeemerTag(Spend, Mint, Cert, Reward))
import Ctl.Internal.Types.RewardAddress (RewardAddress)
import Ctl.Internal.Types.Scripts (MintingPolicyHash)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (findIndex)
import Data.BigInt as BigInt
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)

-- | Redeemer that hasn't yet been indexed, that tracks its purpose info
-- | that is enough to find its index given a `RedeemersContext`.
newtype UnindexedRedeemer = UnindexedRedeemer
  { datum :: PlutusData
  , purpose :: RedeemerPurpose
  }

derive instance Generic UnindexedRedeemer _
derive instance Newtype UnindexedRedeemer _
derive newtype instance Eq UnindexedRedeemer
derive newtype instance EncodeAeson UnindexedRedeemer

instance Show UnindexedRedeemer where
  show = genericShow

-- | A redeemer with an index, but without `ExUnits`
newtype IndexedRedeemer = IndexedRedeemer
  { tag :: RedeemerTag
  , datum :: PlutusData
  , index :: Int
  }

derive instance Generic IndexedRedeemer _
derive instance Newtype IndexedRedeemer _
derive newtype instance Eq IndexedRedeemer
derive newtype instance EncodeAeson IndexedRedeemer

instance Show IndexedRedeemer where
  show = genericShow

-- | Sets `ExUnits` to `zero`
indexedRedeemerToRedeemer :: IndexedRedeemer -> Redeemer
indexedRedeemerToRedeemer (IndexedRedeemer { tag, datum, index }) =
  Redeemer
    { tag
    , index: BigInt.fromInt index
    , data: datum
    , exUnits: { mem: zero, steps: zero }
    }

-- | Contains a value redeemer corresponds to, different for each possible
-- | `RedeemerTag`.
data RedeemerPurpose
  = ForSpend TransactionInput
  | ForMint MintingPolicyHash
  | ForReward RewardAddress
  | ForCert Certificate

derive instance Generic RedeemerPurpose _
derive instance Eq RedeemerPurpose

instance EncodeAeson RedeemerPurpose where
  encodeAeson = case _ of
    ForSpend txo -> encodeAeson { tag: "ForSpend", value: encodeAeson txo }
    ForMint mps -> encodeAeson { tag: "ForMint", value: encodeAeson mps }
    ForReward addr -> encodeAeson { tag: "ForReward", value: encodeAeson addr }
    ForCert cert -> encodeAeson { tag: "ForCert", value: encodeAeson cert }

instance Show RedeemerPurpose where
  show = genericShow

-- | Contains parts of a transaction that are important when indexing redeemers
type RedeemersContext =
  { inputs :: Array TransactionInput
  , mintingPolicyHashes :: Array MintingPolicyHash
  , rewardAddresses :: Array RewardAddress
  , certs :: Array Certificate
  }

mkRedeemersContext :: Transaction -> RedeemersContext
mkRedeemersContext
  (Transaction { body: TxBody { inputs, mint, withdrawals, certs } }) =
  { inputs: Set.toUnfoldable inputs
  , mintingPolicyHashes: Set.toUnfoldable (Map.keys mintedAssets) <#>
      currencyMPSHash
  , rewardAddresses: Set.toUnfoldable $ Map.keys $ fromMaybe Map.empty
      withdrawals
  , certs: fold certs
  }
  where
  mintedAssets = fromMaybe Map.empty (map unwrap $ map unwrap mint)

indexRedeemers
  :: RedeemersContext
  -> Array UnindexedRedeemer
  -> Maybe (Array IndexedRedeemer)
indexRedeemers ctx redeemers = do
  traverse (indexRedeemer ctx) redeemers

indexRedeemer :: RedeemersContext -> UnindexedRedeemer -> Maybe IndexedRedeemer
indexRedeemer ctx (UnindexedRedeemer { purpose, datum }) = case purpose of
  ForSpend input -> findIndex (eq input) ctx.inputs <#> \index ->
    IndexedRedeemer { tag: Spend, index, datum }
  ForMint mps -> findIndex (eq mps) ctx.mintingPolicyHashes <#> \index ->
    IndexedRedeemer { tag: Mint, index, datum }
  ForReward addr -> findIndex (eq addr) ctx.rewardAddresses <#> \index ->
    IndexedRedeemer { tag: Reward, index, datum }
  ForCert cert -> findIndex (eq cert) ctx.certs <#> \index ->
    IndexedRedeemer { tag: Cert, index, datum }
