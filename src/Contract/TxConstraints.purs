-- | A module for building `TxConstraints` te pair with the `ScriptLookups`
-- | as part of an off-chain transaction.
module Contract.TxConstraints (module TxConstraints) where

import Ctl.Internal.Types.TxConstraints
  ( DatumPresence(DatumInline, DatumWitness)
  , InputConstraint(InputConstraint)
  , InputWithScriptRef(RefInput, SpendInput)
  , OutputConstraint(OutputConstraint)
  , TxConstraints(TxConstraints)
  , addTxIn
  , isSatisfiable
  , mustBeSignedBy
  , mustDelegateStakePlutusScript
  , mustDelegateStakePubKey
  , mustDeregisterStakePlutusScript
  , mustDeregisterStakePubKey
  , mustHashDatum
  , mustIncludeDatum
  , mustMintCurrency
  , mustMintCurrencyUsingScriptRef
  , mustMintCurrencyWithRedeemer
  , mustMintCurrencyWithRedeemerUsingScriptRef
  , mustMintValue
  , mustMintValueWithRedeemer
  , mustNotBeValid
  , mustPayToNativeScript
  , mustPayToPubKey
  , mustPayToPubKeyAddress
  , mustPayToPubKeyAddressWithDatum
  , mustPayToPubKeyAddressWithDatumAndScriptRef
  , mustPayToPubKeyAddressWithScriptRef
  , mustPayToPubKeyWithDatum
  , mustPayToPubKeyWithDatumAndScriptRef
  , mustPayToPubKeyWithScriptRef
  , mustPayToScript
  , mustPayToScriptWithScriptRef
  , mustProduceAtLeast
  , mustProduceAtLeastTotal
  , mustReferenceOutput
  , mustRegisterPool
  , mustRegisterStakePubKey
  , mustSatisfyAnyOf
  , mustSpendAtLeast
  , mustSpendAtLeastTotal
  , mustSpendNativeScriptOutput
  , mustSpendPubKeyOutput
  , mustSpendScriptOutput
  , mustSpendScriptOutputUsingScriptRef
  , mustValidateIn
  , mustWithdrawStakePubKey
  , pubKeyPayments
  , requiredDatums
  , requiredMonetaryPolicies
  , requiredSignatories
  , singleton
  ) as TxConstraints
