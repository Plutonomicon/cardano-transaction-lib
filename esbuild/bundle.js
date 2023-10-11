import * as esbuild from "esbuild";
import { buildOptions } from "./config.js";

if (process.argv.length < 4) {
  throw `usage: nodejs bundle.js ENTRY_POINT OUTPUT_DIR`;
}

esbuild.build(
  buildOptions({
    entryPoint: process.argv[2],
    outputDir: process.argv[3],
  })
);
