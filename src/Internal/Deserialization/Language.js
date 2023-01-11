/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@mlabs-haskell/cardano-serialization-lib-browser");
} else {
  lib = require("@mlabs-haskell/cardano-serialization-lib-nodejs");
}

exports._convertLanguage = langCtors => cslLang => {
  if (cslLang.kind() == lib.LanguageKind.PlutusV1) {
    return langCtors.plutusV1;
  } else if (cslLang.kind() == lib.LanguageKind.PlutusV2) {
    return langCtors.plutusV2;
  } else {
    throw "_convertLanguage: Unsupported language kind: " + cslLang.kind();
  }
};
