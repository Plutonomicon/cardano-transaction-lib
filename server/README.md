# cardano-browser-tx-server

This is a small Haskell project to provide services to the `cardano-browser-tx` frontend that cannot be achieved using Purescript

## Goals

We plan on supporting at least the following features:

- [x] Transaction fee estimation via `Cardano.Api`
- [x] Plutus' `applyArguments` for applying `Data` arguments to scripts
- [x] Obtaining the hash of scripts (e.g. `validatorHash`, etc...)

## Development

Run `nix develop .#hsDevShell` (or equivalently `nix develop .#package.x86_64-{linux|darwin}`; NB: not currently tested on macOS) in the repository root (i.e. up one level from `server`). This will place you in a development shell with `cabal`, `hoogle`, `haskell-language-server`, etc...

The server executable can be built with `nix build .#cardano-browser-tx-server:exe:cardano-browser-tx-server` and run with `./result/bin/cardano-browser-tx-server`. `cabal` can also be used once in the development shell. The server will run on port 8081. You can optionally pass the `--port`/`-p` flag to explicitly choose a port to run on

---

# API docs

## GET /fees

### GET Parameters:

- tx
  - **Values**: _84a300818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960001818258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d5336100021a0002b569a0f5f6_
  - **Description**: A CBOR-encoded `Tx AlonzoEra`; should be sent as a hexadecimal string

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

## POST /apply-args

### Request:

- Supported content types are:

  - `application/json;charset=utf-8`
  - `application/json`

- Both the `script` and each of its `args` should be hex-encoded CBOR (`application/json;charset=utf-8`, `application/json`):

```javascript
{"args":["01"],"script": "590a9a0100003233322232323232323322323322323233322233322233322233223233322232323322323233223233333222223322"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

  - `application/json;charset=utf-8`
  - `application/json`

- The applied script will be returned as hex-encoded CBOR (`application/json;charset=utf-8`, `application/json`):

```javascript
"590a9a0100003233322232323232323322323322323233322233322233322233223233322232323322323233223233333222223322"
```

## POST /finalize

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- The input should contain CBOR of Tx, Redeemers and individual plutus datums (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tx":"00","redeemers":"00","datums":["00"]}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- The output is CBOR-encoded Tx (`application/json;charset=utf-8`, `application/json`):

```javascript
"84a300818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960001818258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d5336100021a0002b569a0f5f6"
```

## POST /hash-script

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- The script should be CBOR-encoded hex (`application/json;charset=utf-8`, `application/json`):

```javascript
"4d01000033222220051200120011"
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- The hashed script will be returned as a JSON object with a `getScriptHash` field containing the script hash (`application/json;charset=utf-8`, `application/json`):

```javascript
{"getScriptHash":"67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"}
```
