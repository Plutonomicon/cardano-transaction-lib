# ctl-server

This is a small Haskell project to provide services to the `cardano-transaction-lib` frontend that cannot be achieved using Purescript

## Goals

We plan on supporting at least the following features:

- [x] Transaction fee estimation via `Cardano.Api`
- [x] Plutus' `applyArguments` for applying `Data` arguments to scripts
- [x] Obtaining the hash of scripts (e.g. `validatorHash`, etc...)

## Development

Run `nix develop .#hsDevShell` (or equivalently `nix develop .#package.x86_64-{linux|darwin}`; NB: not currently tested on macOS) in the repository root (i.e. up one level from `server`). This will place you in a development shell with `cabal`, `hoogle`, `haskell-language-server`, etc...

The server executable can be built with `nix build .#cardano-trasaction-lib-server:exe:cardano-trasaction-lib-server` and run with `./result/bin/cardano-trasaction-lib-server`. `cabal` can also be used once in the development shell. The server will run on port 8081. You can optionally pass the `--port`/`-p` flag to explicitly choose a port to run on

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

## POST /blake2b

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Bytes to hash as hexadecimal string (`application/json;charset=utf-8`, `application/json`):

```javascript
"666f6f"
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Hash bytes are returned as hexidecimal string (`application/json;charset=utf-8`, `application/json`):

```javascript
"b8fe9f7f6255a6fa08f668ab632a8d081ad87983c77cd274e48ce450f0b349fd"
```

## GET /eval-ex-units

### GET Parameters:

- tx
    - **Values**: *84a300818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960001818258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d5336100021a0002b569a0f5f6*
    - **Description**: A CBOR-encoded `Tx AlonzoEra`; should be sent as a hexadecimal string


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- The `(RdmrPtr -> ExUnits)` map will be returned as a list of `RdmrPtrExUnits` objects with the following structure (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"exUnitsSteps":0,"rdmrPtrTag":0,"exUnitsMem":0,"rdmrPtrIdx":0}]
```

## GET /fees

### GET Parameters:

- wit-count
    - **Values**: *1*
    - **Description**: A natural number representing the intended number of key witnessesfor the transaction

- tx
    - **Values**: *84a300818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960001818258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d5336100021a0002b569a0f5f6*
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

## POST /finalize

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- The input should contain CBOR of tx, redeemers, individual Plutusdatums, and Plutus script hashes (`application/json;charset=utf-8`, `application/json`):

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

## POST /hash-data

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- The input should contain CBOR of a single datum (`application/json;charset=utf-8`, `application/json`):

```javascript
"00"
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- The data will be returned as hex-encoded CBOR (`application/json;charset=utf-8`, `application/json`):

```javascript
"3834613330303831383235383230356436373732363566613562623231636536643863373530326163613730623933313664313065393538363131663363366237353866363561643935393939363030303138313832353833393030333066623362383533393935316532366630333439313061356133376632326362393964393464316434303966363964646261656139373131633132663033633165663265393335616363333565633265366639366336353066643362666261336539363535303530346435333336313030303231613030303262353639613066356636"
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
