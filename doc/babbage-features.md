# CTL Babbage feature overview

This document is a reference/explainer for the new CTL APIs introduced for Babbage support.

## Reference Inputs

[Reference inputs](https://cips.cardano.org/cips/cip31/#referenceinputs) allow looking at an output without spending it in Plutus scripts.

In CTL reference inputs can be used by providing a value of type `InputWithScriptRef` that specifies whether an output should be spent or referenced. One of the constraints that use it is `mustSpendScriptOutputUsingScriptRef`.

[Usage example](../examples/PlutusV2/ReferenceInputs.purs)

## Reference Scripts

[Reference Scripts](https://developers.cardano.org/docs/governance/cardano-improvement-proposals/cip-0033/) allow to use the scripts without attaching them to the transaction (and using a reference instead).

In CTL, reference scripts can be utilized by first creating a reference point for the script to be used later via `mustPayToScriptWithScriptRef` (or its variants).

This constraint utilises a new `ScriptRef` type that includes either a native script or a Plutus script.

Then, `mustSpendScriptOutputUsingScriptRef` (or its variants) can be used to use a reference script. It accepts a value of type `InputWithScriptRef` that specifies whether the UTxO with the reference script should be spent or referenced.

[Usage example](../examples/PlutusV2/ReferenceScripts.purs)

## Inline Data

[CIP-32](https://developers.cardano.org/docs/governance/cardano-improvement-proposals/cip-0032/) introduces inline data feature that allows to store datum values directly in transaction outputs, instead of storing just the hashes.

In CTL, alternating between datum storage options can be achieved by specifying a `DatumPresence` value with constraints that accept it, like `mustPayToPubKeyWithDatum`.

[Usage example](../examples/PlutusV2/InlineDatum.purs)

## Collateral Output

To trigger a [collateral return](https://cips.cardano.org/cips/cip40/), `mustNotBeValid` constraint should be explicitly specified (otherwise a script error would be detected earlier and the transaction will not be sent).
