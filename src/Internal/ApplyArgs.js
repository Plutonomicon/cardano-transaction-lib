/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

const uplc = require("uplc");

/**
 * @param {} left
 * @param {} right
 * @param {PlutusData} args
 * @param {PlutusScript} script
 * @returns {Either String PlutusScript}
 */
exports.apply_params_to_script = left => right => args => script => {
  let version = script.language_version();
  let appliedScript;
  try {
    let scriptBytes = script.bytes(); // raw bytes
    let argsBytes = args.to_bytes(); // cbor

    try {
      appliedScript = uplc.apply_params_to_script_no_panic(
        argsBytes,
        scriptBytes
      );
    } catch (e) {
      return left("Error applying argument to script: ".concat(e.toString()));
    }
  } catch (e1) {
    return left("Error serializing arguments");
  }
  return right(lib.PlutusScript.new_with_version(appliedScript, version));
};
