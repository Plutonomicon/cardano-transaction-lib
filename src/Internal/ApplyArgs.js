/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

const uplc = require("uplc");

/**
* @param {Uint8Array} params_bytes  -- parameter list encoded as PlutusData List
* @param {Uint8Array} plutus_script_bytes
* @returns {Uint8Array}
* uplc.apply_params_to_script;
*/

/**
* @param {PlutusData} args
* @param {PlutusScript} script
* @returns {PlutusScript}
*/
exports.apply_params_to_script = args => script => {
    try {
    let scriptBytes = script.to_bytes()
    let argsBytes = args.to_bytes()
    let appliedScript
    try { 
        appliedScript = uplc.apply_params_to_script(argsBytes, scriptBytes)
    } catch (e) {
        console.log(e)
        throw e
    }
    return lib.PlutusScript.from_bytes(appliedScript)
    } catch (e1) {
        console.log(e1)
        throw e1
    }
}

/**
* @param {} left
* @param {} right
* @param {PlutusData} args
* @param {PlutusScript} script
* @returns {Either String PlutusScript}
*/
exports.apply_params_to_script_with_errors = left => right => args => script => {
  let version = script.language_version();
  let appliedScript;
  try {
    let scriptBytes = script.bytes(); // raw bytes
    let argsBytes = args.to_bytes(); // cbor

    try { 
        appliedScript = uplc.apply_params_to_script_with_errors(argsBytes, scriptBytes);
    } catch (e) {
        return left("Error applying argument to script: ".concat(e.toString()));
    }

  } catch (e1) {
    return left("Error serializing arguments");
  }
  return right(lib.PlutusScript.new_with_version(appliedScript, version));
}