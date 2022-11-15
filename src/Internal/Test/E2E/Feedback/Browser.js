exports._pushBrowserEvent = message => () => {
  if (typeof window.ctlE2ECommunications != "object") {
    window.ctlE2ECommunications = [];
  }
  window.ctlE2ECommunications.push(message);
};

exports._getClusterSetup = maybe => () =>
  window.ctlE2EClusterSetup
    ? maybe.just(window.ctlE2EClusterSetup)
    : maybe.nothing;
