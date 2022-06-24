var rawData = require('Scripts/always-succeeds.plutus');

var data = JSON.parse(rawData);

exports.alwaysSucceedsCbor = data["cborHex"];