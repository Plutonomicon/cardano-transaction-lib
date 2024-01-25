export function _pushBrowserEvent(message) {
  return () => {
    if (typeof window.ctlE2ECommunications != "object") {
      window.ctlE2ECommunications = [];
    }
    window.ctlE2ECommunications.push(message);
  };
}

export function _getClusterSetup(maybe) {
  return () =>
    window.ctlE2EClusterSetup
      ? maybe.just(window.ctlE2EClusterSetup)
      : maybe.nothing;
}
