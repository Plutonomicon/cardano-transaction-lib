let upstream =
    -- https://github.com/mlabs-haskell/purescript-cardano-package-set
      https://raw.githubusercontent.com/mlabs-haskell/purescript-cardano-package-set/v2.0.0/packages.dhall
        sha256:89e383ba2cceff5b668cefae59aae352e60fb28543f9dc3fb198a0231d56d8e0

let additions =
      { cardano-transaction-balancer =
        { dependencies =
          [ "aff"
          , "ansi"
          , "arrays"
          , "bifunctors"
          , "bytearrays"
          , "cardano-data-lite"
          , "cardano-kupmios-provider"
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
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer"
        , version = "742ad56c7f09e821266806aad31050788a397a08"
        }
      }

in  upstream // additions
