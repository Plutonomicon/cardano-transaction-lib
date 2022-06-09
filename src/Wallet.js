/* global require exports BROWSER_RUNTIME */

// _enableNami :: Effect (Promise Cip30Connection)
exports._enableNami = () =>
    window.cardano.nami.enable();

// _enableGero :: Effect (Promise Cip30Connection)
exports._enableGero = () =>
    window.cardano.gerowallet.enable();
