{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "bigints"
  , "console"
  , "const"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "identity"
  , "maybe"
  , "medea"
  , "mote"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "refs"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
