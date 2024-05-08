module Contract.Constraints where

import Prelude

import Cardano.Types.Epoch (Epoch(..))
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash(..))
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PoolParams (PoolParams(..))
import Cardano.Types.PoolPubKeyHash (PoolPubKeyHash)
import Cardano.Types.ScriptHash (ScriptHash(..))
import Cardano.Types.StakePubKeyHash (StakePubKeyHash)
import Cardano.Types.TransactionInput (TransactionInput(..))
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput(..))
import Ctl.Internal.Types.RedeemerDatum (RedeemerDatum)
import Ctl.Internal.Types.TxConstraints (TxConstraint(..), TxConstraints(..))
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- Spending

-- | Datums in UTxOs can be stored in two forms: inline datums or datum hashes.
-- | When there's a hash, we need to provide a datum corresponding to this hash,
-- | which can be done by either providing the value literally, or using a
-- | reference input where it is stored inline.
data DatumWitness
  = DatumValue PlutusData
  | DatumReference TransactionInput RefInputAction

-- | Gives the user options for specifying everything needed to spend an UTxO
-- | located at an address with a ScriptHash payment credential.
-- |
-- | - `ScriptValue` contains a script for the witness set.
-- |
-- | - `ScriptReference` contains a CIP-31 reference input where the inline script should be available at, and a flag to either spend the referenced input or just reference it.
data ScriptWitness a
  = ScriptValue a
  | ScriptReference TransactionInput RefInputAction

-- | Inputs can be referenced or spent in a transaction (See CIP-31).
-- | Inline datums (CIP-32) and reference scripts (CIP-33) contained within
-- | transaction outputs become visible to the script context of the
-- | transaction, regardless of whether the output is spent or just
-- | referenced. This data type lets the developer to specify, which
-- | action to perform with a referenced input.
data RefInputAction
  = ReferenceInput
  | SpendInput

data SpendWitness
  = PubKeyOutput
  | NativeScriptOutput (ScriptWitness NativeScript)
  | PlutusScriptOutput (ScriptWitness PlutusScript) RedeemerDatum
      (Maybe DatumWitness)

data StakeWitness
  = PubKeyHashStakeWitness StakePubKeyHash
  | PlutusScriptStakeWitness (ScriptWitness PlutusScript)
  | NativeScriptStakeWitness (ScriptWitness NativeScript)

data StakeRegistration
  = StakePubKeyRegistration StakePubKeyHash
  | StakeScriptRegistration ScriptHash

data StakeAction
  = Delegate PoolPubKeyHash
  | Withdraw
  | Deregister

data Constraint
  = SpendOutput TransactionUnspentOutput SpendWitness
  | RegisterStake StakeRegistration
  | Stake StakeAction StakeWitness
  | RequireSignature PaymentPubKeyHash
  | RegisterPool PoolParams
  | RetirePool PoolPubKeyHash Epoch

type Constraints = Array Constraint

requireSignature :: PaymentPubKeyHash -> TxConstraints
requireSignature = singleton <<< MustBeSignedBy

-- Stake Pool operations

registerPool :: PoolParams -> TxConstraints
registerPool = singleton <<< MustRegisterPool

retirePool
  :: PoolPubKeyHash
  -> Epoch
  -> TxConstraints
retirePool poolPubKeyHash = singleton <<< MustRetirePool poolPubKeyHash

-- Stake operations

-- registerStake :: StakeRegistration -> TxConstraints
-- registerStake = singleton <<< case _ of
--   StakePubKeyRegistration spkh -> MustRegisterStakePubKey spkh
--   StakeScriptRegistration ssh -> MustRegisterStakeScript ssh

-- delegateStake :: PoolPubKeyHash -> StakeDelegation -> TxConstraints
-- delegateStake ppkh = singleton <<< case _ of
--   DelegatePubKeyHashStake spkh ->
--     MustDelegateStakePubKey spkh ppkh
--   DelegateNativeScriptStake ns ->
--     MustDelegateStakeNativeScript ns ppkh
--   DelegatePlutusScriptStake ps redeemer ->
--     MustDelegateStakePlutusScript ps redeemer ppkh

-- withdrawStake :: StakeWithdrawal -> TxConstraints
-- withdrawStake = singleton <<< case _ of
--   WithdrawNativeScriptStake ns ->
--     MustWithdrawStakeNativeScript ns
--   WithdrawPlutusScriptStake ps redeemer ->
--     MustWithdrawStakePlutusScript ps redeemer
--   WithdrawPubKeyStake spkh ->
--     MustWithdrawStakePubKey spkh

-- data StakeDelegation
--   = DelegatePubKeyHashStake StakePubKeyHash
--   | DelegateNativeScriptStake NativeScriptWitness
--   | DelegatePlutusScriptStake PlutusScriptWitness

-- data StakeWithdrawal
--   = WithdrawNativeScriptStake NativeScriptWitness
--   | WithdrawPlutusScriptStake PlutusScriptWitness
--   | WithdrawPubKeyStake StakePubKeyHash

-- data DeregisterStake
--   = DeregisterNativeScriptStake NativeScriptWitness
--   | DeregisterPlutusScriptStake PlutusScriptWitness
--   | DeregisterPubKeyStake StakePubKeyHash

-- deregisterStake :: DeregisterStake -> TxConstraints
-- deregisterStake = singleton <<< case _ of
--   DeregisterNativeScriptStake ns ->
--     MustDeregisterStakeNativeScript ns
--   DeregisterPlutusScriptStake ps redeemer ->
--     MustDeregisterStakePlutusScript ps redeemer
--   DeregisterPubKeyStake spkh ->
--     MustDeregisterStakePubKey spkh
