/* global BROWSER_RUNTIME */

let lib;
let apply_args;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
  apply_args = await import("apply-args-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
  apply_args = await import("apply-args-nodejs");
}

/**
 * @param {} left
 * @param {} right
 * @param {PlutusData} args
 * @param {PlutusScript} script
 * @returns {Either String PlutusScript}
 */
export function apply_params_to_script(left) {
  return right => args => script => {
    let version = script.language_version();
    let appliedScript;
    try {
      let scriptBytes = script.bytes(); // raw bytes
      let argsBytes = args.to_bytes(); // cbor

      try {
        appliedScript = apply_args.apply_params_to_script_no_panic(
          argsBytes,
          scriptBytes
        );
      } catch (e) {
        return left("Error applying argument to script: ".concat(e.toString()));
      }
    } catch (e1) {
      return left("Error serializing arguments: ".concat(e1.toString()));
    }
    return right(lib.PlutusScript.new_with_version(appliedScript, version));
  };
}
