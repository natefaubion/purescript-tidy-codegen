{ name = "tidy-codegen"
, dependencies =
  [ "aff"
  , "ansi"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "console"
  , "control"
  , "dodo-printer"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "free"
  , "identity"
  , "integers"
  , "language-cst-parser"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-child-process"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "node-streams"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "posix-types"
  , "prelude"
  , "record"
  , "safe-coerce"
  , "st"
  , "strings"
  , "tidy"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
