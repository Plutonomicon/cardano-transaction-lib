{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aff"
  , "bifunctors"
  , "bytearrays"
  , "cardano-collateral-select"
  , "cardano-data-lite"
  , "cardano-hd-wallet"
  , "cardano-key-wallet"
  , "cardano-message-signing"
  , "cip30-mock"
  , "cardano-plutus-data-schema"
  , "cardano-transaction-builder"
  , "cardano-transaction-lib"
  , "cardano-types"
  , "cip30-mock"
  , "datetime"
  , "effect"
  , "exceptions"
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
  , "uplc-apply-args"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
