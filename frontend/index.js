"use strict";

const SeabugContracts = require("../seabug_contracts/api.js");

const args = 'test';

console.log("app starting");
SeabugContracts.callMarketPlaceBuyTest(args).then(r => console.log(r));
