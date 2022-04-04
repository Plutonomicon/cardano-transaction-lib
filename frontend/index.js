"use strict";

const SeabugContracts = require("../seabug_contracts/api.js");

const args = 'test';

console.log("app starting");
setTimeout(() => SeabugContracts.callMarketPlaceBuyTest(args).then(r => console.log(r)), 1);
