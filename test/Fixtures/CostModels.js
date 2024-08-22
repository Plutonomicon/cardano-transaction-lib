import * as CSL from "@mlabs-haskell/cardano-serialization-lib-gc";

export function defaultCostmdls() {
  return CSL.TxBuilderConstants.plutus_vasil_cost_models();
}
