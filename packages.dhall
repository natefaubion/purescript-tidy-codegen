let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.3-20210811/packages.dhall sha256:a2de7ef2f2e753733eddfa90573a82da0c7c61d46fa87d015b7f15ef8a6e97d5

let additions =
      { dodo-printer =
        { dependencies =
          [ "ansi", "foldable-traversable", "lists", "maybe", "strings" ]
        , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
        , version = "v2.1.0"
        }
      , language-cst-parser =
        { dependencies =
          [ "arrays"
          , "const"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "free"
          , "functors"
          , "maybe"
          , "numbers"
          , "ordered-collections"
          , "strings"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-language-cst-parser.git"
        , version = "v0.9.1"
        }
      , tidy =
        { dependencies =
          [ "arrays"
          , "dodo-printer"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "language-cst-parser"
          , "strings"
          , "tuples"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-tidy.git"
        , version = "d0ae49b9314f5f4c9b918e3744d287f557142d6e"
        }
      }

in  upstream // additions
