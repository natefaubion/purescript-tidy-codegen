let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220513/packages.dhall
        sha256:1ed784f37ae6131d99acd542d058d5ce39954ccaacc3adba5cc7cf1549d2bffa

in upstream
  with language-cst-parser =
    (upstream.language-cst-parser with version = "v0.13.0")

  with tidy =
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
    , repo = "https://github.com/natefaubion/purescript-tidy.git"
    , version = "v0.10.0"
    }
