{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ "aff", "arrays", "argonaut", "bigints", "console", "control", "debug", "either", "effect", "foldable-traversable", "foreign-object", "generics-rep", "int-53", "maybe", "medea", "mote", "ordered-collections", "prelude", "psci-support", "spec", "strings", "transformers" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
