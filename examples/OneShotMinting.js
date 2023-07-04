/* global BROWSER_RUNTIME */

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/one-shot-minting.plutus");
} else {
  const fs = await import("fs");
  script = fs.readFileSync(
    new URL("../../fixtures/scripts/one-shot-minting.plutus", import.meta.url),
    "utf8"
  );
}

export { script as oneShotMinting };
