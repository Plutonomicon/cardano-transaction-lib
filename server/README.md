# ctl-server

This is a small Haskell project to provide services to the `cardano-transaction-lib` frontend that cannot be achieved using Purescript

## Goals

We plan on supporting at least the following features:

- [x] Transaction fee estimation via `Cardano.Api`
- [x] Plutus' `applyArguments` for applying `Data` arguments to scripts
- [x] ~~Obtaining the hash of scripts (e.g. `validatorHash`, etc...)~~

## Development

Run `nix develop .#hsDevShell` (or equivalently `nix develop .#package.x86_64-{linux|darwin}`; NB: not currently tested on macOS) in the repository root (i.e. up one level from `server`). This will place you in a development shell with `cabal`, `hoogle`, `haskell-language-server`, etc...

The server executable can be built with `nix build .#ctl-server:exe:ctl-server` and run with `./result/bin/ctl-server`. `cabal` can also be used once in the development shell. The server will run on port 8081. You can optionally pass the `--port`/`-p` flag to explicitly choose a port to run on

---

# API docs

## POST /apply-args

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Both the `script` and each of its `args` should be hex-encoded CBOR (`application/json;charset=utf-8`, `application/json`):

```javascript
{"args":["01"],"script":"4d01000033222220051200120011"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- The applied script will be returned as hex-encoded CBOR (`application/json;charset=utf-8`, `application/json`):

```javascript
"4d01000033222220051200120011"
```

## POST /fees

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- The input should contain the intended number of witnesses and theCBOR of the tx (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tx":"00","count":1}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- The `Fee` will be returned encoded as a JSON string (`application/json;charset=utf-8`, `application/json`):

```javascript
"160265"
```
