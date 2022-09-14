-- | A module for building `TxConstraints` te pair with the `ScriptLookups`
-- | as part of an off-chain transaction.
module CTL.Contract.TxConstraints (module TxConstraints) where

import CTL.Internal.Types.TxConstraints
  ( DatumPresence(DatumInline, DatumWitness)
  , InputConstraint(InputConstraint)
  , InputWithScriptRef(RefInput, SpendInput)
  , OutputConstraint(OutputConstraint)
  , TxConstraint
      ( MustIncludeDatum
      , MustValidateIn
      , MustBeSignedBy
      , MustSpendAtLeast
      , MustProduceAtLeast
      , MustSpendPubKeyOutput
      , MustSpendScriptOutput
      , MustSpendNativeScriptOutput
      , MustReferenceOutput
      , MustMintValue
      , MustPayToPubKeyAddress
      , MustPayToScript
      , MustHashDatum
      , MustSatisfyAnyOf
      , MustNotBeValid
      )
  , TxConstraints(TxConstraints)
  , addTxIn
  , isSatisfiable
  , modifiesUtxoSet
  , mustBeSignedBy
  , mustHashDatum
  , mustIncludeDatum
  , mustMintCurrency
  , mustMintCurrencyUsingScriptRef
  , mustMintCurrencyWithRedeemer
  , mustMintCurrencyWithRedeemerUsingScriptRef
  , mustMintValue
  , mustMintValueWithRedeemer
  , mustPayToNativeScript
  , mustNotBeValid
  , mustPayToScript
  , mustPayToScriptWithScriptRef
  , mustPayToPubKey
  , mustPayToPubKeyAddress
  , mustPayToPubKeyAddressWithDatum
  , mustPayToPubKeyAddressWithDatumAndScriptRef
  , mustPayToPubKeyAddressWithScriptRef
  , mustPayToPubKeyWithDatum
  , mustPayToPubKeyWithDatumAndScriptRef
  , mustPayToPubKeyWithScriptRef
  , mustProduceAtLeast
  , mustProduceAtLeastTotal
  , mustReferenceOutput
  , mustSatisfyAnyOf
  , mustSpendAtLeast
  , mustSpendAtLeastTotal
  , mustSpendPubKeyOutput
  , mustSpendScriptOutput
  , mustSpendNativeScriptOutput
  , mustSpendScriptOutputUsingScriptRef
  , mustValidateIn
  , pubKeyPayments
  , requiredDatums
  , requiredMonetaryPolicies
  , requiredSignatories
  , singleton
  ) as TxConstraints
