/* global BROWSER_RUNTIME SCRIPTS_DIR */

export const _loadScript = path => async () => {
  if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
    const response = await fetch(`${SCRIPTS_DIR}/${path}`);
    return await response.text();
  } else {
    const fs = await import("fs");
    const url = new URL(`../../fixtures/scripts/${path}`, import.meta.url);
    return fs.readFileSync(url, "utf8");
  }
};
