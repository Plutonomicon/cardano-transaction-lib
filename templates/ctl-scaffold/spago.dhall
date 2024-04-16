{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aff"
  , "js-bigints"
  , "cardano-transaction-lib"
  , "datetime"
  , "effect"
  , "maybe"
  , "mote"
  , "ordered-collections"
  , "posix-types"
  , "prelude"
  , "spec"
  , "noble-secp256k1"
  , "bytearrays"
  , "cardano-hd-wallet"
  , "cardano-message-signing"
  , "cardano-plutus-data-schema"
  , "cardano-serialization-lib"
  , "cardano-types"
  , "plutus-types"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
