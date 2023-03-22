module Ctl.Internal.ProcessConstraints.UnbalancedTx where

import Prelude hiding (join)

import Contract.Hashing (plutusScriptStakeValidatorHash)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Trans (StateT, get, gets, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Address (addressPaymentValidatorHash)
import Ctl.Internal.BalanceTx.RedeemerIndex
  ( RedeemerPurpose(ForReward, ForCert, ForMint, ForSpend)
  , UnindexedRedeemer(UnindexedRedeemer)
  , unindexedRedeemerToRedeemer
  )
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef(NativeScriptRef))
import Ctl.Internal.Cardano.Types.Transaction
  ( Certificate
      ( StakeDelegation
      , PoolRetirement
      , PoolRegistration
      , StakeDeregistration
      , StakeRegistration
      )
  , Costmdls
  , Transaction
  , TransactionOutput(TransactionOutput)
  , TransactionWitnessSet(TransactionWitnessSet)
  , _body
  , _certs
  , _inputs
  , _isValid
  , _mint
  , _networkId
  , _outputs
  , _referenceInputs
  , _requiredSigners
  , _scriptDataHash
  , _withdrawals
  , _witnessSet
  )
import Ctl.Internal.Cardano.Types.Transaction (Redeemer) as T
import Ctl.Internal.Cardano.Types.Value
  ( Coin(Coin)
  , CurrencySymbol
  , Value
  , getNonAdaAsset
  , isZero
  , mkSingletonValue'
  , mpsSymbol
  , negation
  , split
  )
import Ctl.Internal.Contract (getProtocolParameters)
import Ctl.Internal.Contract.Monad (Contract, getQueryHandle, wrapQueryM)
import Ctl.Internal.Hashing (datumHash) as Hashing
import Ctl.Internal.Helpers (liftM, (<\>))
import Ctl.Internal.IsData (class IsData)
import Ctl.Internal.NativeScripts (nativeScriptHash)
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusTxOutputWithRefScript
  , fromPlutusValue
  )
import Ctl.Internal.Plutus.Types.Credential
  ( Credential(ScriptCredential, PubKeyCredential)
  )
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutputWithRefScript) as Plutus
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Ctl.Internal.QueryM.Pools
  ( getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  )
import Ctl.Internal.Scripts
  ( mintingPolicyHash
  , nativeScriptStakeValidatorHash
  , validatorHash
  , validatorHashEnterpriseAddress
  )
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId
  , StakeCredential
  , baseAddress
  , baseAddressToAddress
  , keyHashCredential
  , scriptHashCredential
  )
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Transaction
  ( ModifyTxError
  , attachDatum
  , attachNativeScript
  , attachPlutusScript
  , setScriptDataHash
  )
import Ctl.Internal.Types.Any (Any)
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Ctl.Internal.Types.Interval
  ( POSIXTimeRange
  , PosixTimeToSlotError
  , always
  , intersection
  , isEmpty
  , posixTimeRangeToTransactionValidity
  )
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  )
import Ctl.Internal.Types.PubKeyHash
  ( PaymentPubKeyHash
  , StakePubKeyHash
  , payPubKeyHashBaseAddress
  , payPubKeyHashEnterpriseAddress
  , stakePubKeyHashRewardAddress
  )
import Ctl.Internal.Types.RewardAddress
  ( stakePubKeyHashRewardAddress
  , stakeValidatorHashRewardAddress
  ) as RewardAddress
import Ctl.Internal.Types.Scripts
  ( MintingPolicy(NativeMintingPolicy, PlutusMintingPolicy)
  , MintingPolicyHash
  , NativeScriptStakeValidator
  , PlutusScriptStakeValidator
  , Validator
  , ValidatorHash
  )
import Ctl.Internal.Types.TokenName (TokenName)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Types.TxConstraints
  ( DatumPresence(DatumWitness, DatumInline)
  , InputConstraint(InputConstraint)
  , InputWithScriptRef(RefInput, SpendInput)
  , OutputConstraint(OutputConstraint)
  , TxConstraint
      ( MustBeSignedBy
      , MustDelegateStakePubKey
      , MustDelegateStakePlutusScript
      , MustDelegateStakeNativeScript
      , MustDeregisterStakePubKey
      , MustDeregisterStakePlutusScript
      , MustDeregisterStakeNativeScript
      , MustHashDatum
      , MustIncludeDatum
      , MustMintValue
      , MustNotBeValid
      , MustPayToNativeScript
      , MustPayToPubKeyAddress
      , MustPayToScript
      , MustProduceAtLeast
      , MustReferenceOutput
      , MustRegisterPool
      , MustRegisterStakePubKey
      , MustRegisterStakeScript
      , MustRetirePool
      , MustSatisfyAnyOf
      , MustSpendAtLeast
      , MustSpendNativeScriptOutput
      , MustSpendPubKeyOutput
      , MustSpendScriptOutput
      , MustValidateIn
      , MustWithdrawStakePubKey
      , MustWithdrawStakePlutusScript
      , MustWithdrawStakeNativeScript
      , MustMintValueUsingNativeScript
      )
  , TxConstraints(TxConstraints)
  , utxoWithScriptRef
  )
import Ctl.Internal.Types.TypedTxOut
  ( TypeCheckError
  , mkTypedTxOut
  , typeTxOutRef
  , typedTxOutDatumHash
  , typedTxOutRefValue
  , typedTxOutTxOut
  )
import Ctl.Internal.Types.TypedValidator
  ( class DatumType
  , class ValidatorTypes
  , TypedValidator(TypedValidator)
  )
import Ctl.Internal.Types.TypedValidator (generalise) as TV
import Ctl.Internal.Types.UnbalancedTransaction (PaymentPubKey)
import Data.Array (cons, partition, toUnfoldable, zip)
import Data.Array (singleton, union, (:)) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), either, hush, isRight, note)
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Lattice (join)
import Data.Lens (non, (%=), (%~), (.=), (.~), (<>=))
import Data.Lens.Getter (to, use)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.List (List(Nil, Cons))
import Data.Map (Map, empty, fromFoldable, lookup, singleton, union)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Set (insert) as Set
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(SProxy))
import Data.Traversable (for, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import MedeaPrelude (mapMaybe)
import Prelude (join) as Bind
import Type.Proxy (Proxy(Proxy))

-- | A newtype for the unbalanced transaction after creating one with datums
-- | and redeemers not attached
newtype UnbalancedTx = UnbalancedTx
  { transaction :: Transaction -- the unbalanced tx created
  , utxoIndex :: Map TransactionInput TransactionOutput
  , datums :: Array Datum -- the array of ordered datums that require attaching
  , redeemers :: Array UnindexedRedeemer
  }

derive instance Generic UnbalancedTx _
derive instance Newtype UnbalancedTx _
derive newtype instance Eq UnbalancedTx
-- derive newtype instance EncodeAeson UnbalancedTx

instance Show UnbalancedTx where
  show = genericShow
