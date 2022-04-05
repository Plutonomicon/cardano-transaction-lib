# seabug-example

A package that uses `cardano-transaction-lib-seabug` as a dependency.

1. Run `npm run bundle-seabug` to bundle PS code. This will prepare `cardano-transaction-lib-seabug` package.
2. Register it as a local package: `cd npm-packages/cardano-transaction-lib-seabug && npm install && npm link .`
3. Make `cardano-transaction-link-seabug` visible to `seabug-example`: run `npm link cardano-transaction-lib-seabug` in this directory.
3. Then, from root directory: `npm run bundle-seabug-example`
4. Statically serve `npm-packages/seabug-example` directory and open `index.html`.
