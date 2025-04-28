let upstream =
    -- https://github.com/mlabs-haskell/purescript-cardano-package-set
      https://raw.githubusercontent.com/mlabs-haskell/purescript-cardano-package-set/v3.0.0/packages.dhall
        sha256:53f8de47606b6cb349432c2f2f03e656b204ebe132ef2d39d76339d9d97620ee

let additions =
      { cardano-transaction-balancer =
        { dependencies =
          [ "aff"
          , "ansi"
          , "arrays"
          , "bifunctors"
          , "bytearrays"
          , "cardano-data-lite"
          , "cardano-provider"
          , "cardano-transaction-builder"
          , "cardano-types"
          , "console"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "integers"
          , "js-bigints"
          , "js-date"
          , "lattice"
          , "lists"
          , "literals"
          , "maybe"
          , "monad-logger"
          , "newtype"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "prelude"
          , "profunctor"
          , "profunctor-lenses"
          , "quickcheck"
          , "random"
          , "strings"
          , "stringutils"
          , "these"
          , "transformers"
          , "tuples"
          , "uint"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer"
        , version = "941c2234ae3d8355dad3282bb51ea6678a587349"
        }
      }

in (upstream // additions)
