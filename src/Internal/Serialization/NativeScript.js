/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}

const mkScript = prop => arg => lib.NativeScript[prop](arg);

export function mkScriptPubkey(keyHash) {
  return lib.ScriptPubkey.new(keyHash);
}

export const nativeScript_new_script_pubkey = mkScript("new_script_pubkey");
export const nativeScript_new_script_all = mkScript("new_script_all");
export const nativeScript_new_script_any = mkScript("new_script_any");
export const nativeScript_new_script_n_of_k = mkScript("new_script_n_of_k");
export const nativeScript_new_timelock_start = mkScript("new_timelock_start");
export const nativeScript_new_timelock_expiry = mkScript("new_timelock_expiry");

export function _packNativeScripts(helper) {
  return nss => helper.pack(lib.NativeScripts, nss);
}

export function mkScriptAll(nss) {
  return lib.ScriptAll.new(nss);
}

export function mkScriptAny(nss) {
  return lib.ScriptAny.new(nss);
}

export function mkScriptNOfK(n) {
  return nss => lib.ScriptNOfK.new(n, nss);
}

export function mkTimelockExpiry(n) {
  return lib.TimelockExpiry.new_timelockexpiry(n);
}

export function mkTimelockStart(n) {
  return lib.TimelockStart.new_timelockstart(n);
}
