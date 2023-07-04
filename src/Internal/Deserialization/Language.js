/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = await import("@emurgo/cardano-serialization-lib-nodejs");
}
// import gcWrapper from "@mlabs-haskell/csl-gc-wrapper";
// lib = gcWrapper(lib);

export function _convertLanguage(langCtors) {
  return cslLang => {
    if (cslLang.kind() == lib.LanguageKind.PlutusV1) {
      return langCtors.plutusV1;
    } else if (cslLang.kind() == lib.LanguageKind.PlutusV2) {
      return langCtors.plutusV2;
    } else {
      throw "_convertLanguage: Unsupported language kind: " + cslLang.kind();
    }
  };
}
