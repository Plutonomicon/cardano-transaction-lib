-- | Redeemer indexing refers to the process of updating redeemer's `index`
-- | value based on its `RedeemerPurpose` and context from the transaction.
-- | Redeemer indexing is needed, because at the Tx construction stage we
-- | don't know the exact indices redeemers will have after balancing.
-- | For the algorithm, see `indexof` description in
-- | "Combining Scripts with Their Inputs" chapter of "A Formal Specification
-- | of the Cardano Ledger integrating Plutus Core"
-- | https://github.com/input-output-hk/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
module Ctl.Internal.BalanceTx.RedeemerIndex
  ( IndexedRedeemer(IndexedRedeemer)
  , RedeemerPurpose(ForReward, ForCert, ForMint, ForSpend)
  , RedeemersContext
  , UnindexedRedeemer(UnindexedRedeemer)
  , attachIndexedRedeemers
  , attachRedeemers
  , indexRedeemers
  , indexedRedeemerToRedeemer
  , mkRedeemersContext
  , unindexedRedeemerToRedeemer
  ) where

import Prelude

import Aeson (class EncodeAeson, encodeAeson)
import Ctl.Internal.Cardano.Types.Transaction
  ( Certificate
  , Redeemer(Redeemer)
  , Transaction(Transaction)
  , TxBody(TxBody)
  , _redeemers
  , _witnessSet
  )
import Ctl.Internal.Cardano.Types.Value (currencyMPSHash, unwrapNonAdaAsset)
import Ctl.Internal.Types.PlutusData (PlutusData)
import Ctl.Internal.Types.RedeemerTag (RedeemerTag(Spend, Mint, Cert, Reward))
import Ctl.Internal.Types.RewardAddress (RewardAddress)
import Ctl.Internal.Types.Scripts (MintingPolicyHash)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (findIndex)
import Data.BigInt as BigInt
import Data.Either (Either, note)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Lens ((.~))
import Data.Map as Map
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (for)

attachRedeemers :: Array Redeemer -> Transaction -> Transaction
attachRedeemers redeemers = _witnessSet <<< _redeemers .~ Just redeemers

attachIndexedRedeemers :: Array IndexedRedeemer -> Transaction -> Transaction
attachIndexedRedeemers = attachRedeemers <<< map indexedRedeemerToRedeemer

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

-- | Ignore the value that the redeemer points to
redeemerPurposeToRedeemerTag :: RedeemerPurpose -> RedeemerTag
redeemerPurposeToRedeemerTag = case _ of
  ForSpend _ -> Spend
  ForMint _ -> Mint
  ForReward _ -> Reward
  ForCert _ -> Cert

unindexedRedeemerToRedeemer :: UnindexedRedeemer -> Redeemer
unindexedRedeemerToRedeemer (UnindexedRedeemer { datum, purpose }) =
  Redeemer
    { tag: redeemerPurposeToRedeemerTag purpose
    , "data": datum
    , index: zero
    , exUnits: zero
    }

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
-- | Allows to uniquely compute redeemer index, given a `RedeemersContext` that
-- | is valid for the transaction.
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
  mintedAssets = fromMaybe Map.empty (map unwrapNonAdaAsset $ map unwrap mint)

indexRedeemers
  :: RedeemersContext
  -> Array UnindexedRedeemer
  -> Either UnindexedRedeemer (Array IndexedRedeemer)
indexRedeemers ctx redeemers = do
  for redeemers \redeemer -> note redeemer $ indexRedeemer ctx redeemer

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
