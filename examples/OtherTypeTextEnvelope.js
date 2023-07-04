/* global BROWSER_RUNTIME */

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/other-type-text-envelope.plutus");
} else {
  const fs = await import("fs");
  script = fs.readFileSync(
    new URL(
      "../../fixtures/scripts/other-type-text-envelope.plutus",
      import.meta.url
    ),
    "utf8"
  );
}
export { script as otherTypeTextEnvelope };
