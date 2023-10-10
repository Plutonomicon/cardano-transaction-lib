"use strict";

// This needs to be asynchronous to load the WASM from cardano-serialization-lib
//
// You also need to call `spago bundle-module` to generate the module that is
// imported here. From the repository root, run:
//   spago bundle-module -m <MAIN> --to output.js
// Normally it should be done via Nix (see flake.nix) or via a Makefile
import("../output.js").then(m => m.main());
