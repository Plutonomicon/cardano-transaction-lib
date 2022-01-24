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
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "gen"
  , "identity"
  , "maybe"
  , "medea"
  , "mote"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "refs"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
