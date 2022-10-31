const fs = require("fs");
//const path = require("path");

exports._rmdirSync = path => () => fs.rmdirSync(path, { recursive: true });

// exports._removeDirs = dirPath => () => {
//   const dirs = fs.readdirSync(dirPath);
//   for (const dir of dirs) {
//     fs.rmdirSync(path.resolve(dirPath, dir), { recursive: true });
//   }
// };
