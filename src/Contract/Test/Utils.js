export function exitCode(code) {
  return () => {
    process.exitCode = code;
  };
}
