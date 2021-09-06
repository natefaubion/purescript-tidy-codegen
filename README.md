# purescript-tidy-codegen

Super convenient code-generation for PureScript using `purescript-tidy`.

## Introduction

`tidy-codegen` provides constructors for quickly building CST types from
`purescript-language-cst-parser`. Paired with `tidy`, code-generators can
be assembled in hardly any time at all.

```purescript
import Prelude
import PureScript.CST.Types
import Tidy.Codegen
import Tidy.Codegen.Monad

import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

exampleModule :: Module Void
exampleModule = unsafePartial $ codegenModule "Data.Maybe" do
  importOpen "Prelude"
  tell
    [ declData "Maybe" [ typeVar "a" ]
        [ dataCtor "Nothing" []
        , dataCtor "Just" [ typeVar "a" ]
        ]

    , declDerive Nothing []
        (typeApp "Functor" [ typeVar "a" ])

    , declSignature "maybe" do
        typeForall [ typeVar "a", typeVar "b" ] do
          typeArrow
            [ typeVar "b"
            , typeArrow [ typeVar "a" ] (typeVar "b")
            , typeApp (typeCtor "Maybe") [ typeVar "a" ]
            ]
            (typeVar "b")
    , declValue "maybe" [ binderVar "nothing", binderVar "just" ] do
        exprCase [ exprSection ]
          [ caseBranch [ binderCtor "Just" [ bindarVar "a" ] ] do
              exprApp (exprIdent "just") [ exprIdent "a" ]
          , caseBranch [ binderCtor "Nothing" [] ] do
              exprIdent "nothing"
          ]
    ]
```
```purescript
module Data.Maybe where

import Prelude

data Maybe a = Nothing | Just a

derive instance Functor Maybe

maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe nothing just = case _ of
  Just a -> just a
  Nothing -> nothing
```

## A note on overloading and `Partial`

`tidy-codegen` is designed to be fast to write. It features a highly overloaded
API which lets you intuitively construct CST types like it's a bare-bones AST.
It's common to use string and array literals, where the CST types actually use
more specific newtypes or non-empty arrays. In these cases, the overloaded APIs
will do runtime validation (eg. by lexing string literals) and crash if these
arguments are incorrect. This results in a `Partial` constraint which must be
discharged with `unsafePartial`. If you want to avoid this partiality, you can
always pass in the safe types instead with all overloaded APIs and no `Partial`
constraint will be required.

## Examples

All of the [snapshot modules](./test/snapshots) are self contained examples,
with their results shown in `.output` files.
