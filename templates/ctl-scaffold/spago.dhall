{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aff"
  , "bytearrays"
  , "cardano-hd-wallet"
  , "cardano-plutus-data-schema"
  , "cardano-collateral-select"
  , "cardano-key-wallet"
  , "cardano-message-signing"
  , "cip30-mock"
  , "uplc-apply-args"
  , "cardano-serialization-lib"
  , "cardano-transaction-builder"
  , "cardano-transaction-lib"
  , "cardano-types"
  , "datetime"
  , "effect"
  , "js-bigints"
  , "maybe"
  , "mote"
  , "mote-testplan"
  , "noble-secp256k1"
  , "ordered-collections"
  , "plutus-types"
  , "posix-types"
  , "prelude"
  , "safely"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
