/* global BROWSER_RUNTIME */

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/always-mints.plutus");
} else {
  const fs = await import("fs");
  script = fs.readFileSync(
    new URL("../../fixtures/scripts/always-mints.plutus", import.meta.url),
    "utf8"
  );
}
export { script as alwaysMints };
