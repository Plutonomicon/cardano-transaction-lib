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
  , "control"
  , "debug"
  , "either"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "identity"
  , "maybe"
  , "medea"
  , "mote"
  , "node-buffer"
  , "node-fs-aff"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "refs"
  , "spec"
  , "strings"
  , "transformers" 
  , "undefined"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  ]
}
