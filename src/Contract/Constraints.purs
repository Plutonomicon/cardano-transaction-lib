module Contract.Constraints where

import Prelude

import Cardano.Types.Certificate (Certificate)
import Cardano.Types.Coin (Coin)
import Cardano.Types.Epoch (Epoch)
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PoolParams (PoolParams)
import Cardano.Types.PoolPubKeyHash (PoolPubKeyHash)
import Cardano.Types.StakeCredential (StakeCredential)
import Cardano.Types.StakePubKeyHash (StakePubKeyHash)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Ctl.Internal.Types.RedeemerDatum (RedeemerDatum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

type Constraints = Array Constraint

data Constraint
  = SpendOutput TransactionUnspentOutput OutputWitness
  | RegisterStake StakeCredential
  | IssueCertificate Certificate CredentialWitness
  | WithdrawStake StakeCredential Coin CredentialWitness
  | RequireSignature PaymentPubKeyHash
  | RegisterPool PoolParams
  | RetirePool PoolPubKeyHash Epoch

derive instance Generic Constraint _
instance Show Constraint where
  show = genericShow

-- | `OutputWitness` is used to provide the evidence needed to consume an
-- | output. It must correspond to a `TransactionUnspentOutput` address'
-- | payment credential to unlock it.
data OutputWitness
  = PubKeyOutput
  | NativeScriptOutput (ScriptWitness NativeScript)
  | PlutusScriptOutput (ScriptWitness PlutusScript) RedeemerDatum
      (Maybe DatumWitness)

derive instance Generic OutputWitness _
instance Show OutputWitness where
  show = genericShow

-- | `CredentialWitness` is used to provide the evidence needed to perform
-- | operations on behalf of a credential, which include:
-- |
-- | - Minting
-- | - Certificate witnessing
-- | - Rewards withdrawal
-- |
-- | Unlike `OutputWitness`, it does not include a `DatumWitness`, because
-- | minting policies and stake scripts do not have a datum.
data CredentialWitness
  = PubKeyCredential
  | NativeScriptCredential (ScriptWitness NativeScript)
  | PlutusScriptCredential (ScriptWitness PlutusScript) RedeemerDatum

derive instance Generic CredentialWitness _
instance Show CredentialWitness where
  show = genericShow

-- | Gives the user options for specifying everything needed to spend an UTxO
-- | located at an address with a ScriptHash payment credential.
-- |
-- | - `ScriptValue` contains a script for the witness set.
-- |
-- | - `ScriptReference` contains a CIP-31 reference input where the inline script should be available at, and a flag to either spend the referenced input or just reference it.
data ScriptWitness a
  = ScriptValue a
  | ScriptReference TransactionInput RefInputAction

derive instance Show a => Generic (ScriptWitness a) _
instance Show a => Show (ScriptWitness a) where
  show = genericShow

-- | Inputs can be referenced or spent in a transaction (See CIP-31).
-- | Inline datums (CIP-32) and reference scripts (CIP-33) contained within
-- | transaction outputs become visible to the script context of the
-- | transaction, regardless of whether the output is spent or just
-- | referenced. This data type lets the developer to specify, which
-- | action to perform with a referenced input.
data RefInputAction
  = ReferenceInput
  | SpendInput

derive instance Generic RefInputAction _
instance Show RefInputAction where
  show = genericShow

-- | Datums in UTxOs can be stored in two forms: inline datums or datum hashes.
-- | When there's a hash, we need to provide a datum corresponding to this hash,
-- | which can be done by either providing the value literally, or using a
-- | reference input where it is stored inline.
data DatumWitness
  = DatumValue PlutusData
  | DatumReference TransactionInput RefInputAction

derive instance Generic DatumWitness _
instance Show DatumWitness where
  show = genericShow

data StakeWitness
  = PubKeyHashStakeWitness StakePubKeyHash
  | PlutusScriptStakeWitness (ScriptWitness PlutusScript)
  | NativeScriptStakeWitness (ScriptWitness NativeScript)

derive instance Generic StakeWitness _
instance Show StakeWitness where
  show = genericShow

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
