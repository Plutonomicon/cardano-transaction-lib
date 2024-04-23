import { runContract } from "ctl-scaffold";
import type {} from "ctl-scaffold";

function delay(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

(async () => {
  await delay(1000); // need some time for cardano object to be injected
  console.log("Weclome to ctl-scaffold demo!");
  await runContract();
})();
