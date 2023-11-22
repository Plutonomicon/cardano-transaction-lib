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
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
