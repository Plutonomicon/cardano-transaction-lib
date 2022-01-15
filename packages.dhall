{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211116/packages.dhall sha256:7ba810597a275e43c83411d2ab0d4b3c54d0b551436f4b1632e9ff3eb62e327a

let additions =
      { mote =
          { dependencies = [ "these", "transformers", "arrays" ]
          , repo = "https://github.com/garyb/purescript-mote"
          , version = "v1.1.0"
          }
      , medea =
          { dependencies =   
            [ "aff"
            , "argonaut"
            , "arrays"
            , "bifunctors"
            , "control"
            , "effect"
            , "either"
            , "enums"
            , "exceptions"
            , "foldable-traversable"
            , "foreign-object"
            , "free"
            , "integers"
            , "lists"
            , "maybe"
            , "mote"
            , "naturals"
            , "newtype"
            , "node-buffer"
            , "node-fs-aff"
            , "node-path"
            , "nonempty"
            , "ordered-collections"
            , "parsing"
            , "partial"
            , "prelude"
            , "psci-support"
            , "quickcheck"
            , "quickcheck-combinators"
            , "safely"
            , "spec"
            , "strings"
            , "these"
            , "transformers"
            , "typelevel"
            , "tuples"
            , "unicode"
            , "unordered-collections"
            , "unsafe-coerce"
            ]
        , repo = "https://github.com/juspay/medea-ps.git"
        , version = "8b215851959aa8bbf33e6708df6bd683c89d1a5a"
        }
      }

in  upstream // additions
