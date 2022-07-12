/* global BROWSER_RUNTIME */

const getNativeScriptAs = prop => maybe => ns => {
  const res = ns[prop]();
  return res == null ? maybe.nothing : maybe.just(res);
};

const call = property => object => object[property]();

exports.getScriptPubkey = getNativeScriptAs("as_script_pubkey");
exports.getScriptAll = getNativeScriptAs("as_script_all");
exports.getScriptAny = getNativeScriptAs("as_script_any");
exports.getScriptNOfK = getNativeScriptAs("as_script_n_of_k");
exports.getTimelockStart = getNativeScriptAs("as_timelock_start");
exports.getTimelockExpiry = getNativeScriptAs("as_timelock_expiry");
exports.scriptPubkey_addr_keyhash = call("addr_keyhash");
exports.scriptAllScripts = helper =>
  helper.unpackFromProperty("native_scripts");
exports.scriptAnyScripts = helper =>
  helper.unpackFromProperty("native_scripts");
exports.scriptNOfKScripts = helper =>
  helper.unpackFromProperty("native_scripts");
exports.scriptNOfK_n = call("n");
exports.timelockStart_slot = call("slot_bignum");
exports.timelockExpiry_slot = call("slot_bignum");
