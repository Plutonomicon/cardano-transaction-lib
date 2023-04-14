module Ctl.Internal.ProcessConstraints.Error where

import Prelude

import Ctl.Internal.Cardano.Types.Value (CurrencySymbol)
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Transaction (ModifyTxError)
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Ctl.Internal.Types.Interval (POSIXTimeRange, PosixTimeToSlotError)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash, StakePubKeyHash)
import Ctl.Internal.Types.Scripts
  ( MintingPolicy
  , MintingPolicyHash
  , NativeScriptStakeValidator
  , PlutusScriptStakeValidator
  , Validator
  , ValidatorHash
  )
import Ctl.Internal.Types.TokenName (TokenName)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Types.TypedTxOut (TypeCheckError)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data MkUnbalancedTxError
  = CannotConvertPaymentPubKeyHash PaymentPubKeyHash
  | CannotFindDatum
  | CannotQueryDatum DataHash
  | CannotConvertPOSIXTimeRange POSIXTimeRange PosixTimeToSlotError
  | CannotSolveTimeConstraints POSIXTimeRange POSIXTimeRange
  | CannotGetMintingPolicyScriptIndex -- Should be impossible
  | CannotGetValidatorHashFromAddress Address -- Get `ValidatorHash` from internal `Address`
  | CannotHashDatum Datum
  | CannotHashMintingPolicy MintingPolicy
  | CannotHashValidator Validator
  | CannotMakeValue CurrencySymbol TokenName BigInt
  | CannotWithdrawRewardsPubKey StakePubKeyHash
  | CannotWithdrawRewardsPlutusScript PlutusScriptStakeValidator
  | CannotWithdrawRewardsNativeScript NativeScriptStakeValidator
  | DatumNotFound DataHash
  | DatumWrongHash DataHash Datum
  | MintingPolicyHashNotCurrencySymbol MintingPolicyHash
  | MintingPolicyNotFound MintingPolicyHash
  | ModifyTx ModifyTxError
  | OwnPubKeyAndStakeKeyMissing
  | TxOutRefNotFound TransactionInput
  | TxOutRefWrongType TransactionInput
  | TypeCheckFailed TypeCheckError
  | TypedTxOutHasNoDatumHash
  | TypedValidatorMissing
  | ValidatorHashNotFound ValidatorHash
  | WrongRefScriptHash (Maybe ScriptHash)
  | CannotSatisfyAny
  | ExpectedPlutusScriptGotNativeScript MintingPolicyHash
  | CannotMintZero CurrencySymbol TokenName

derive instance Generic MkUnbalancedTxError _
derive instance Eq MkUnbalancedTxError

instance Show MkUnbalancedTxError where
  show = genericShow
