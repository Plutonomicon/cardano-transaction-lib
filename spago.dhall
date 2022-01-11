{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "argonaut"
  , "bigints"
  , "console"
  , "const"
  , "control"
  , "debug"
  , "either"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "identity"
  , "lists"
  , "maybe"
  , "medea"
  , "mote"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
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
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  ]
}
