/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}

export function _convertLanguage(langCtors) {
  return cslLang => {
    if (cslLang.kind() == lib.LanguageKind.PlutusV1) {
      return langCtors.plutusV1;
    } else if (cslLang.kind() == lib.LanguageKind.PlutusV2) {
      return langCtors.plutusV2;
    } else if (cslLang.kind() == lib.LanguageKind.PlutusV3) {
      return langCtors.plutusV3;
    } else {
      throw "_convertLanguage: Unsupported language kind: " + cslLang.kind();
    }
  };
}
