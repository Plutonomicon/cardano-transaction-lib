"use strict";

// This needs to be asynchronous to load the WASM from CSL
//
// You also need to call `spago bundle-module` to generate the module that is
// imported here:
//   spago bundle-module -m <MAIN> --to spago-bundle.js
import("./spago-bundle.js").then((m) => m.main());
