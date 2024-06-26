{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aff"
  , "bytearrays"
  , "cardano-hd-wallet"
  , "cardano-message-signing"
  , "cardano-plutus-data-schema"
  , "cardano-serialization-lib"
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
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
