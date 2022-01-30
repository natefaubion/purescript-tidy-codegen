{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "language-cst-codegen"
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
