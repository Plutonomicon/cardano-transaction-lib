import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";

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
