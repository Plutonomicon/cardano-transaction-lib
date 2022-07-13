var rawData1 = require("Scripts/redeemer1.plutus");
var rawData2 = require("Scripts/redeemer2.plutus");
var rawData3 = require("Scripts/redeemer3.plutus");

var data1 = JSON.parse(rawData1);
var data2 = JSON.parse(rawData2);
var data3 = JSON.parse(rawData3);

exports.mintingPolicyRdmrInt1Cbor = data1["cborHex"];
exports.mintingPolicyRdmrInt2Cbor = data2["cborHex"];
exports.mintingPolicyRdmrInt3Cbor = data3["cborHex"];
