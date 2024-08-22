# CTL Babbage feature overview

This document is a reference/explainer for the new CTL APIs introduced for Babbage support.

**Table of contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Reference Inputs & Reference Scripts](#reference-inputs--reference-scripts)
- [Inline Data](#inline-data)
- [Collateral Output](#collateral-output)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->
## Reference Inputs & Reference Scripts

[Reference inputs](https://cips.cardano.org/cip/CIP-0031#reference-inputs) allow looking at an output without spending it in Plutus scripts.

[Reference Scripts](https://cips.cardano.org/cip/CIP-0033) allows the use of scripts without attaching them to the transaction (and using a reference instead).

Reference scripts can be utilized in CTL by first creating a UTxO containing the script to be used later.

[Usage example](../examples/PlutusV2/ReferenceInputsAndScripts.purs)

## Inline Data

[CIP-32](https://cips.cardano.org/cip/CIP-0032) introduces the inline data feature that allows storing datum values directly in transaction outputs, instead of just the hashes.

## Collateral Output

[CIP-40](https://cips.cardano.org/cip/CIP-0040) introduces explicit collateral output. On validation failure, previously the entire collateral was consumed. Now, if excess collateral is supplied, even with native assets, the surplus can be returned on validation failure.

Collateral output is automatically added to transactions in CTL. To trigger a collateral return, the `mustNotBeValid` constraint should be explicitly specified, otherwise a script error would be detected earlier and the transaction will not be sent.

[Usage example](../examples/Lose7Ada.purs)
