"use strict";

exports.clearLineHandler = readline => () => {
  readline.removeAllListners("line");
};
