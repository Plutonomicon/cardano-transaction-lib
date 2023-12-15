import * as esbuild from "esbuild";
import { buildOptions } from "./config.js";

if (process.argv.length < 4) {
  throw `usage: node bundle.js ENTRY_POINT OUTPUT_FILENAME`;
}

esbuild.build(
  buildOptions({
    entryPoint: process.argv[2],
    outfile: process.argv[3],
  }),
);
