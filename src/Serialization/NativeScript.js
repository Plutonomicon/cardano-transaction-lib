/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@ngua/cardano-serialization-lib-browser');
} else {
    lib = require('@ngua/cardano-serialization-lib-nodejs');
}

const mkScript = prop => arg => lib.NativeScript[prop](arg);

exports.mkScriptPubkey = keyHash => lib.ScriptPubkey.new(keyHash);
exports.nativeScript_new_script_pubkey = mkScript('new_script_pubkey');
exports.nativeScript_new_script_all = mkScript('new_script_all');
exports.nativeScript_new_script_any = mkScript('new_script_any');
exports.nativeScript_new_script_n_of_k = mkScript('new_script_n_of_k');
exports.nativeScript_new_timelock_start = mkScript('new_timelock_start');
exports.nativeScript_new_timelock_expiry = mkScript('new_timelock_expiry');
exports.packNativeScripts = helper => nss => helper.pack(lib.NativeScripts, nss);
exports.mkScriptAll = nss => lib.ScriptAll.new(nss);
exports.mkScriptAny = nss => lib.ScriptAny.new(nss);
exports.mkScriptNOfK = n => nss => lib.ScriptNOfK.new(n, nss);
exports.mkTimelockExpiry = n => lib.TimelockExpiry.new(n);
exports.mkTimelockStart = n => lib.TimelockStart.new(n);
