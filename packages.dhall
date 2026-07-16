let upstream =
    -- https://github.com/mlabs-haskell/purescript-cardano-package-set
      https://raw.githubusercontent.com/mlabs-haskell/purescript-cardano-package-set/v4.1.0/packages.dhall
        sha256:f8d7c3ff5aea758f64cd0876e5be3f8b778b447579b615f143c4043744bd5e37

let additions =
      { cardano-blockfrost-provider =
        { dependencies =
          [ "aeson"
          , "aff"
          , "affjax"
          , "arrays"
          , "bifunctors"
          , "bignumber"
          , "bytearrays"
          , "cardano-provider"
          , "cardano-data-lite"
          , "cardano-types"
          , "datetime"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "http-methods"
          , "js-bigints"
          , "js-date"
          , "maybe"
          , "media-types"
          , "monad-logger"
          , "newtype"
          , "numbers"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "prelude"
          , "strings"
          , "transformers"
          , "tuples"
          , "uint"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cardano-blockfrost-provider"
        , version = "7ecf6b4facc7a3e1ff6cec7f83051fe5b1808d26"
        }
      }

in (upstream // additions)
