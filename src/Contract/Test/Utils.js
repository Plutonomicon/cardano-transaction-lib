exports.exitCode = code => () => {
  process.exitCode = code;
};
