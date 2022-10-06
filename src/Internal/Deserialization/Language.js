/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

// foreign import _convertLanguage
//   :: forall r.ErrorFfiHelper r -> { plutusV1 :: Language, plutusV2 :: Language } -> CSL.Language -> E r Language
exports._convertLanguage = errorHelper => langCtors => cslLang => {
  try {
    if (cslLang.kind() == lib.LanguageKind.PlutusV1) {
      return errorHelper.valid(langCtors.plutusV1);
    } else if (cslLang.kind() == lib.LanguageKind.PlutusV2) {
      return errorHelper.valid(langCtors.plutusV2);
    } else {
      return errorHelper.error(
        "_convertLanguage: Unsupported language kind: " + cslLang.kind()
      );
    }
  } catch (e) {
    return errorHelper.error("_convertLanguage raised: " + e);
  }
};
