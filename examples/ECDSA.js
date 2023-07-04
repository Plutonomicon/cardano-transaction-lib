/* global BROWSER_RUNTIME */

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/validate-ecdsa.plutus");
} else {
  const fs = await import("fs");
  script = fs.readFileSync(
    new URL("../../fixtures/scripts/validate-ecdsa.plutus", import.meta.url),
    "utf8"
  );
}
export { script as validateECDSA };
