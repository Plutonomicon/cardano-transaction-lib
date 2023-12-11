# CTL Babbage feature overview

This document is a reference/explainer for the new CTL APIs introduced for Babbage support.

**Table of contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Reference Inputs](#reference-inputs)
- [Reference Scripts](#reference-scripts)
- [Inline Data](#inline-data)
- [Collateral Output](#collateral-output)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->
## Reference Inputs

[Reference inputs](https://cips.cardano.org/cip/CIP-0031#reference-inputs) allow looking at an output without spending it in Plutus scripts.

There are two ways to use an input as a reference in the constraints API:

1. via `mustReferenceOutput`, which allows Plutus scripts to access the information (e.g. datum, locked value) contained in the output.

[Usage example](../examples/PlutusV2/ReferenceInputs.purs)

2. by providing constraints which accept a value of the type `InputWithScriptRef` with the `RefInput` constructor. These allow scripts (validating or minting) to be reused by reference between multiple transactions without including them in those transactions, explained further in [Reference Scripts](#reference-scripts).

[Usage example](../examples/PlutusV2/ReferenceInputsAndScripts.purs)

## Reference Scripts

[Reference Scripts](https://cips.cardano.org/cip/CIP-0033) allows the use of scripts without attaching them to the transaction (and using a reference instead).

Reference scripts can be utilized in CTL by first creating a reference point for the script to be used later via `mustPayToScriptWithScriptRef` (or its variants).

This constraint utilises a new `ScriptRef` type that includes either a native script or a Plutus script.

Then, `mustSpendScriptOutputUsingScriptRef` (or its variants) can be used to use a reference script. It accepts a value of type `InputWithScriptRef` that specifies whether the UTxO with the reference script should be spent or referenced.

[Usage example](../examples/PlutusV2/ReferenceScripts.purs)

## Inline Data

[CIP-32](https://cips.cardano.org/cip/CIP-0032) introduces the inline data feature that allows storing datum values directly in transaction outputs, instead of just the hashes.

In CTL, alternating between datum storage options can be achieved by specifying a `DatumPresence` value with constraints that accept it, like `mustPayToPubKeyWithDatum`.

[Usage example](../examples/PlutusV2/InlineDatum.purs)

## Collateral Output

[CIP-40](https://cips.cardano.org/cip/CIP-0040) introduces explicit collateral output. On validation failure, previously the entire collateral was consumed. Now, if excess collateral is supplied, even with native assets, the surplus can be returned on validation failure.

Collateral output is automatically added to transactions in CTL. To trigger a collateral return, the `mustNotBeValid` constraint should be explicitly specified, otherwise a script error would be detected earlier and the transaction will not be sent.
