{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aff"
  , "bigints"
  , "cardano-transaction-lib"
  , "effect"
  , "mote"
  , "ordered-collections"
  , "prelude"
  , "spec"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
