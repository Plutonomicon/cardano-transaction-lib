{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arraybuffer-types"
  , "arrays"
  , "bifunctors"
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
  , "lists"
  , "maybe"
  , "medea"
  , "mote"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "refs"
  , "spec"
  , "strings"
  , "these"
  , "transformers"
  , "tuples"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
