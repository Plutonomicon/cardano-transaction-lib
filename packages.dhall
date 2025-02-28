let upstream =
    -- https://github.com/mlabs-haskell/purescript-cardano-package-set
      https://raw.githubusercontent.com/mlabs-haskell/purescript-cardano-package-set/v1.2.0/packages.dhall
        sha256:1879aeee12ef41d5f39ed8b530efa817c747366553b2fc90981ad4e8c21fc5d8

let additions =
      { cardano-kupmios-provider =
        { dependencies =
          [ "aeson"
          , "aff"
          , "aff-promise"
          , "affjax"
          , "arrays"
          , "bifunctors"
          , "bytearrays"
          , "cardano-key-wallet"
          , "cardano-provider"
          , "cardano-serialization-lib"
          , "cardano-types"
          , "console"
          , "control"
          , "datetime"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "formatters"
          , "http-methods"
          , "integers"
          , "js-bigints"
          , "js-date"
          , "lists"
          , "maybe"
          , "monad-logger"
          , "newtype"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "prelude"
          , "profunctor-lenses"
          , "record"
          , "strings"
          , "stringutils"
          , "tailrec"
          , "these"
          , "transformers"
          , "tuples"
          , "uint"
          , "untagged-union"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-kupmios-provider"
        , version = "bcaadf9b37c4c9290b77579d212fd37b06730632"
        }
      , cardano-ogmios-mempool-provider =
        { dependencies =
          [ "aeson"
          , "aff"
          , "argonaut-codecs"
          , "arrays"
          , "bifunctors"
          , "bytearrays"
          , "cardano-kupmios-provider"
          , "cardano-provider"
          , "cardano-types"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "lists"
          , "maybe"
          , "monad-logger"
          , "newtype"
          , "ordered-collections"
          , "prelude"
          , "record"
          , "refs"
          , "transformers"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-ogmios-mempool-provider"
        , version = "788cb701e5aaad1e299a118eb07a4f4fc6d5c734"
        }
      }

in  upstream // additions
