/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}

export function _convertNativeScript(handler) {
  return ns => {
    switch (ns.kind()) {
      case lib.NativeScriptKind.ScriptPubkey:
        return handler.scriptPubkey(ns.as_script_pubkey());
      case lib.NativeScriptKind.ScriptAll:
        return handler.scriptAll(ns.as_script_all());
      case lib.NativeScriptKind.ScriptAny:
        return handler.scriptAny(ns.as_script_any());
      case lib.NativeScriptKind.ScriptNOfK:
        return handler.scriptNOfK(ns.as_script_n_of_k());
      case lib.NativeScriptKind.TimelockStart:
        return handler.timelockStart(ns.as_timelock_start());
      case lib.NativeScriptKind.TimelockExpiry:
        return handler.timelockExpiry(ns.as_timelock_expiry());
      default:
        throw "Impossible native script kind: " + ns.kind();
    }
  };
}

const call = property => object => object[property]();

export const scriptPubkey_addr_keyhash = call("addr_keyhash");

export function scriptAllScripts(helper) {
  return helper.unpackFromProperty("native_scripts");
}

export function scriptAnyScripts(helper) {
  return helper.unpackFromProperty("native_scripts");
}

export function scriptNOfKScripts(helper) {
  return helper.unpackFromProperty("native_scripts");
}

export const scriptNOfK_n = call("n");
export const timelockStart_slot = call("slot_bignum");
export const timelockExpiry_slot = call("slot_bignum");
