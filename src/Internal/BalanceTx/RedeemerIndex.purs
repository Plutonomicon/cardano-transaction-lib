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
import Cardano.Types
  ( Certificate
  , ExUnits(ExUnits)
  , Redeemer(Redeemer)
  , RewardAddress
  , Transaction(Transaction)
  , TransactionBody(TransactionBody)
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.RedeemerTag (RedeemerTag(Spend, Mint, Cert, Reward))
import Cardano.Types.TransactionInput (TransactionInput)
import Ctl.Internal.Types.Scripts (MintingPolicyHash)
import Data.Array (findIndex)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Lens ((.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Type.Proxy (Proxy(..))

attachRedeemers :: Array Redeemer -> Transaction -> Transaction
attachRedeemers redeemers =
  _Newtype <<< prop (Proxy :: Proxy "witnessSet") <<<
  _Newtype <<< prop (Proxy :: Proxy "redeemers") .~ redeemers

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
    , index: BigNum.zero
    , exUnits: ExUnits { mem: BigNum.zero, steps: BigNum.zero }
    }

-- | A redeemer with an index, but without `ExUnits`
newtype IndexedRedeemer = IndexedRedeemer
  { tag :: RedeemerTag
  , datum :: PlutusData
  , index :: Prim.Int
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
    , index: BigNum.fromInt index
    , data: datum
    , exUnits: ExUnits { mem: BigNum.zero, steps: BigNum.zero }
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
  (Transaction { body: TransactionBody { inputs, mint, withdrawals, certs } }) =
  { inputs: inputs
  , mintingPolicyHashes:
      map wrap $ Set.toUnfoldable $ Map.keys $ unwrap $ fromMaybe
        (wrap Map.empty)
        mint
  , rewardAddresses: Set.toUnfoldable $ Map.keys $ withdrawals
  , certs
  }

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
