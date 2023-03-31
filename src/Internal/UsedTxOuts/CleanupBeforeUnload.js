exports._attachCleanupHandler = nothing => just => callback => () => {
  // If not in the browser, exit
  if (typeof window === "undefined") return nothing;
  // Attach beforeunload handler
  try {
    window.addEventListener("beforeunload", callback);
    return just(() => {
      window.removeEventListener("beforeunload", callback);
    });
  } catch (e) {
    console.log(
      "cardano-transaction-lib: Failed to attach `beforeunload` handler"
    );
    return nothing;
  }
};
