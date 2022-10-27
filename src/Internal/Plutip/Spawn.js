"use strict";

exports.clearLineHandler = readline => () => {
  readline.removeAllListeners("line");
};
