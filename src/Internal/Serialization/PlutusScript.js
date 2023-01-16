/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}
lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);

exports.newPlutusV1Script = bytes => lib.PlutusScript.new(bytes);

exports.newPlutusV2Script = bytes => lib.PlutusScript.new_v2(bytes);

exports.plutusScriptBytes = script => script.bytes();
