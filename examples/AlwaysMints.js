var rawData = require("Scripts/always-mints.plutus");

var data = JSON.parse(rawData);

exports.alwaysMintsCbor = data["cborHex"];
